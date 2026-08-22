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

(defpackage :candlesticks/data-source-kinds
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config)
  (:export :data-source-kind
           :data-source-kind-id
           :data-source-kind-name
           :data-source-kind-description
           :data-source-kind-p
           :make-data-source-kind
           :data-source-kind-by-name
           :data-source-kind-id-for
           :all-data-source-kinds
           :standard-data-source-kinds
           :ensure-standard-data-source-kinds))
(in-package :candlesticks/data-source-kinds)

(defclass data-source-kind ()
  ((id
    :initarg :id
    :accessor data-source-kind-id
    :type string
    :initform ""
    :documentation "The UUID of this data source kind.")
   (name
    :initarg :name
    :accessor data-source-kind-name
    :type string
    :initform ""
    :documentation "The canonical name, e.g. \"api\".")
   (description
    :initarg :description
    :accessor data-source-kind-description
    :initform nil
    :documentation "An optional human-readable description."))
  (:documentation
   "A classification of how Candlesticks talks to a data source: api,
   website, feed, file, or manual."))

(defun data-source-kind-p (object)
  "True if OBJECT is a DATA-SOURCE-KIND."
  (typep object 'data-source-kind))

(defmethod print-object ((kind data-source-kind) stream)
  (print-unreadable-object (kind stream :type t)
    (format stream "~A" (data-source-kind-name kind))))

(defun row->data-source-kind (row)
  "Build a DATA-SOURCE-KIND from a row of (id name description)."
  (make-instance 'data-source-kind
                 :id (first row)
                 :name (second row)
                 :description (from-db (third row))))

(defparameter *standard-data-source-kinds*
  '(("api" "Programmatic HTTP/RPC API")
    ("website" "HTML or other page we scrape or parse")
    ("feed" "Push feed (websocket, FIX, ...)")
    ("file" "File or dump (CSV, JSON, ...)")
    ("manual" "Entered or corrected by hand"))
  "The conventional data source kinds, as (name description) pairs.")

(defun standard-data-source-kinds ()
  "The conventional (name description) data-source-kind pairs.  A fresh copy
   is returned, so callers may mutate the result freely."
  (mapcar #'(lambda (d) (copy-list d)) *standard-data-source-kinds*))

(defun make-data-source-kind (name &key description)
  "Find or create the DATA-SOURCE-KIND with the given NAME and return it.
   NAME is canonicalized (trimmed and down-cased).  DESCRIPTION, when given,
   fills in a missing description on an existing kind."
  (let ((canonical (canonicalize-slug name)))
    (let ((row (postmodern:query
                "insert into data_source_kinds (name, description)
                 values ($1, $2)
                 on conflict (name) do update
                   set description = coalesce(data_source_kinds.description,
                                              excluded.description)
                 returning id, name, description"
                canonical (to-db description) :row)))
      (row->data-source-kind row))))

(defun data-source-kind-by-name (name)
  "The DATA-SOURCE-KIND whose canonical name is NAME, or NIL."
  (let ((row (first-row
              "select id, name, description from data_source_kinds
               where name = $1 limit 1"
              (canonicalize-slug name))))
    (and row (row->data-source-kind row))))

(defun data-source-kind-id-for (kind)
  "The database UUID of KIND, or NIL when KIND is NIL.  KIND may be a
   DATA-SOURCE-KIND object or a name string (which is found or created)."
  (cond ((null kind) nil)
        ((data-source-kind-p kind)
         (data-source-kind-id kind))
        ((stringp kind)
         (data-source-kind-id (make-data-source-kind kind)))
        (t (error "Cannot resolve the data source kind ~S" kind))))

(defun all-data-source-kinds ()
  "All data source kinds, alphabetically by name."
  (mapcar #'row->data-source-kind
          (postmodern:query
           "select id, name, description from data_source_kinds order by name"
           :rows)))

(defun ensure-standard-data-source-kinds ()
  "Insert the standard api/website/feed/file/manual kinds if they are not
   already present, and return them."
  (mapcar (lambda (d)
            (make-data-source-kind (first d) :description (second d)))
          *standard-data-source-kinds*))

(behavior 'standard-data-source-kinds
  (should= 5 (length *standard-data-source-kinds*))
  (should-equal '("api" "Programmatic HTTP/RPC API")
                (first *standard-data-source-kinds*))
  (should-equal '("manual" "Entered or corrected by hand")
                (fifth *standard-data-source-kinds*))
  (let ((copy (standard-data-source-kinds)))
    (setf (first (first copy)) "mutated")
    (should-string= "api" (first (first *standard-data-source-kinds*)))))

(behavior 'data-source-kind
  (let ((kind (make-instance 'data-source-kind
                             :id "k1" :name "api"
                             :description "Programmatic HTTP/RPC API")))
    (should-be-a 'data-source-kind kind)
    (should-be-true (data-source-kind-p kind))
    (should-string= "k1" (data-source-kind-id kind))
    (should-string= "api" (data-source-kind-name kind))
    (should-string= "Programmatic HTTP/RPC API"
                    (data-source-kind-description kind))
    (should-string= "#<DATA-SOURCE-KIND api>"
                    (with-output-to-string (s) (princ kind s)))))
