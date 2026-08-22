;;;; Copyright (c) 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NO LIMIT TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage :candlesticks/data-retrievals
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :candlesticks/data-sources
        :candlesticks/exchanges)
  (:export :data-retrieval
           :data-retrieval-id
           :data-retrieval-source-id
           :data-retrieval-source
           :data-retrieval-source-name
           :data-retrieval-exchange-id
           :data-retrieval-exchange
           :data-retrieval-exchange-name
           :data-retrieval-retrieved-at
           :data-retrieval-endpoint
           :data-retrieval-notes
           :record-retrieval
           :retrieval-by-id
           :retrievals-for-source))
(in-package :candlesticks/data-retrievals)

(defclass data-retrieval ()
  ((id
    :initarg :id
    :accessor data-retrieval-id
    :type string
    :initform ""
    :documentation "The UUID of this retrieval.")
   (source-id
    :initarg :source-id
    :accessor data-retrieval-source-id
    :type string
    :initform ""
    :documentation "The UUID of the data source this came from.")
   (exchange-id
    :initarg :exchange-id
    :accessor data-retrieval-exchange-id
    :initform nil
    :documentation "The UUID of the exchange these bars are for, or NIL
   when the retrieval is not pinned to a venue.")
   (exchange
    :initarg :exchange
    :accessor data-retrieval-exchange
    :initform nil
    :documentation "The EXCHANGE these bars are for, or NIL.")
   (retrieved-at
    :initarg :retrieved-at
    :accessor data-retrieval-retrieved-at
    :documentation "A universal time: when the data was retrieved.")
   (endpoint
    :initarg :endpoint
    :accessor data-retrieval-endpoint
    :initform nil
    :documentation "An optional endpoint (e.g. a URL) that was queried.")
   (notes
    :initarg :notes
    :accessor data-retrieval-notes
    :initform nil
    :documentation "Optional free-form notes about the retrieval."))
  (:documentation
   "A single act of retrieving a set of data from a data source, optionally
   for a particular exchange: when it happened, the endpoint used, and any
   notes.  Every candlestick points back to the retrieval that produced it."))

(defmethod print-object ((retrieval data-retrieval) stream)
  (print-unreadable-object (retrieval stream :type t)
    (format stream "from ~A at ~A" (data-retrieval-source-id retrieval)
            (data-retrieval-retrieved-at retrieval))))

(defparameter *retrieval-select*
  "select r.id, r.data_source_id, r.exchange_id, r.retrieved_at,
          r.endpoint, r.notes, e.name
   from data_retrievals r
   left join exchanges e on e.id = r.exchange_id"
  "The SELECT used to load a retrieval together with its exchange name.")

(defun row->data-retrieval (row)
  "Build a DATA-RETRIEVAL from a joined row of
   (id source-id exchange-id retrieved-at endpoint notes exchange-name)."
  (let ((exchange-id (from-db (third row))))
    (make-instance 'data-retrieval
                   :id (first row)
                   :source-id (second row)
                   :exchange-id exchange-id
                   :retrieved-at (fourth row)
                   :endpoint (from-db (fifth row))
                   :notes (from-db (sixth row))
                   :exchange (and exchange-id
                                  (make-instance 'exchange
                                                 :id exchange-id
                                                 :name (seventh row))))))

(defun record-retrieval (source-id &key (retrieved-at (get-universal-time))
                         exchange endpoint notes)
  "Record that a set of data was retrieved from the data source with the given
   SOURCE-ID, optionally for EXCHANGE, and return the new DATA-RETRIEVAL."
  (let ((exchange-id (exchange-id-for exchange)))
    (let ((id (postmodern:query
               "insert into data_retrievals
                    (data_source_id, exchange_id, retrieved_at, endpoint, notes)
                values ($1, $2, $3, $4, $5)
                returning id"
               source-id (to-db exchange-id)
               (universal-time->timestamptz retrieved-at)
               (to-db endpoint) (to-db notes)
               :single)))
      (retrieval-by-id id))))

(defun retrieval-by-id (id)
  "The DATA-RETRIEVAL with the given UUID, or NIL."
  (let ((row (first-row
              (concatenate 'string *retrieval-select* " where r.id = $1")
              id)))
    (and row (row->data-retrieval row))))

(defun data-retrieval-source (retrieval)
  "The DATA-SOURCE that the RETRIEVAL came from."
  (data-source-by-name (data-retrieval-source-name retrieval)))

(defun data-retrieval-source-name (retrieval)
  "The name of the data source that the RETRIEVAL came from."
  (let ((row (first-row
              "select name from data_sources
               where id = (select data_source_id from data_retrievals
                           where id = $1)"
              (data-retrieval-id retrieval))))
    (and row (first row))))

(defmethod data-retrieval-exchange-name ((retrieval data-retrieval))
  "The canonical name of the exchange this RETRIEVAL is pinned to, or NIL."
  (let ((ex (data-retrieval-exchange retrieval)))
    (and ex (exchange-name ex))))

(defun retrievals-for-source (source-id &key (limit 100))
  "The most recent retrievals from the data source with the given SOURCE-ID."
  (mapcar #'row->data-retrieval
          (postmodern:query
           (concatenate 'string *retrieval-select*
                        " where r.data_source_id = $1
                          order by r.retrieved_at desc
                          limit $2")
           source-id limit :rows)))

(behavior 'data-retrieval
  (let ((r (make-instance 'data-retrieval
                          :id "r1" :source-id "s1"
                          :retrieved-at 1700000000
                          :endpoint "https://example.com"
                          :notes "test")))
    (should-be-a 'data-retrieval r)
    (should-string= "r1" (data-retrieval-id r))
    (should-string= "s1" (data-retrieval-source-id r))
    (should= 1700000000 (data-retrieval-retrieved-at r))
    (should-string= "https://example.com" (data-retrieval-endpoint r))
    (should-string= "test" (data-retrieval-notes r))))
