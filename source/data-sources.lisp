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
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage :candlesticks/data-sources
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :candlesticks/data-source-kinds)
  (:export :data-source
           :data-source-id
           :data-source-name
           :data-source-kind
           :data-source-kind-id
           :data-source-kind-name
           :make-data-source
           :data-source-by-name
           :data-source-id-for
           :all-data-sources))
(in-package :candlesticks/data-sources)

(defclass data-source ()
  ((id
    :initarg :id
    :accessor data-source-id
    :type string
    :initform ""
    :documentation "The UUID of this data source.")
   (name
    :initarg :name
    :accessor data-source-name
    :type string
    :initform ""
    :documentation "The source's name, unique, e.g. \"CoinGecko\".")
   (data-source-kind-id
    :initarg :data-source-kind-id
    :accessor data-source-kind-id
    :initform nil
    :documentation "The UUID of this source's kind, or NIL.")
   (data-source-kind
    :initarg :data-source-kind
    :accessor data-source-kind
    :initform nil
    :documentation "The DATA-SOURCE-KIND of this source, or NIL."))
  (:documentation
   "A source of financial data: CoinGecko, Zapper.fi, Yahoo Finance, or
   whatever else the data actually came from."))

(defmethod print-object ((source data-source) stream)
  (print-unreadable-object (source stream :type t)
    (format stream "~A" (data-source-name source))))

(defmethod data-source-kind-name ((source data-source))
  "The canonical name of SOURCE's kind, or NIL."
  (let ((kind (data-source-kind source)))
    (and kind (data-source-kind-name kind))))

(defparameter *data-source-select*
  "select s.id, s.name, k.id, k.name, k.description
   from data_sources s
   left join data_source_kinds k on k.id = s.data_source_kind_id"
  "The SELECT used to load a data source together with its kind.")

(defun row->data-source (row)
  "Build a DATA-SOURCE from a joined row of
   (id name kind-id kind-name kind-description)."
  (let* ((kind-id (from-db (third row)))
         (kind (and kind-id
                    (make-instance 'data-source-kind
                                   :id kind-id
                                   :name (fourth row)
                                   :description (from-db (fifth row))))))
    (make-instance 'data-source
                   :id (first row)
                   :name (second row)
                   :data-source-kind-id kind-id
                   :data-source-kind kind)))

(defun data-source-by-id (id)
  "The DATA-SOURCE with the given UUID, or NIL."
  (let ((row (first-row
              (concatenate 'string *data-source-select* " where s.id = $1")
              id)))
    (and row (row->data-source row))))

(defun make-data-source (name &key kind)
  "Find or create the DATA-SOURCE with the given NAME and return it.  KIND
   is a DATA-SOURCE-KIND or a name such as \"api\"; when given, it fills in
   a missing kind on an existing source."
  (let ((kind-id (data-source-kind-id-for kind)))
    (let ((id (postmodern:query
               "insert into data_sources (name, data_source_kind_id)
                values ($1, $2)
                on conflict (name) do update
                  set data_source_kind_id = coalesce(data_sources.data_source_kind_id,
                                                     excluded.data_source_kind_id)
                returning id"
               name (to-db kind-id)
               :single)))
      (data-source-by-id id))))

(defun data-source-by-name (name)
  "The DATA-SOURCE with the given NAME, or NIL."
  (let ((row (first-row
              (concatenate 'string *data-source-select*
                           " where s.name = $1 limit 1")
              name)))
    (and row (row->data-source row))))

(defun data-source-id-for (name &optional kind)
  "The database UUID of the data source called NAME, created if it does not
   exist yet.  KIND, when given, is filled in on an existing source whose
   kind is missing."
  (data-source-id (make-data-source name :kind kind)))

(defun all-data-sources ()
  "All data sources, alphabetically by name."
  (mapcar #'row->data-source
          (postmodern:query
           (concatenate 'string *data-source-select* " order by s.name")
           :rows)))

(behavior 'data-source
  (let* ((kind (make-instance 'data-source-kind :id "k1" :name "api"))
         (src (make-instance 'data-source
                             :id "s1" :name "CoinGecko"
                             :data-source-kind-id "k1"
                             :data-source-kind kind)))
    (should-be-a 'data-source src)
    (should-string= "s1" (data-source-id src))
    (should-string= "CoinGecko" (data-source-name src))
    (should-string= "k1" (data-source-kind-id src))
    (should-string= "api" (data-source-kind-name src))))
