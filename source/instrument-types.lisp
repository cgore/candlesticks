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

(defpackage :candlesticks/instrument-types
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config)
  (:export :instrument-type
           :instrument-type-id
           :instrument-type-name
           :instrument-type-description
           :instrument-type-p
           :make-instrument-type
           :instrument-type-by-name
           :instrument-type-id-for
           :all-instrument-types
           :standard-instrument-types
           :ensure-standard-instrument-types))
(in-package :candlesticks/instrument-types)

(defclass instrument-type ()
  ((id
    :initarg :id
    :accessor instrument-type-id
    :type string
    :initform ""
    :documentation "The UUID of this instrument type.")
   (name
    :initarg :name
    :accessor instrument-type-name
    :type string
    :initform ""
    :documentation "The canonical name, e.g. \"cryptocurrency\".")
   (description
    :initarg :description
    :accessor instrument-type-description
    :initform nil
    :documentation "An optional human-readable description."))
  (:documentation
   "A classification of what an instrument is: stock, bond, cryptocurrency,
   commodity, currency, index, fund, and so on."))

(defun instrument-type-p (object)
  "True if OBJECT is an INSTRUMENT-TYPE."
  (typep object 'instrument-type))

(defmethod print-object ((instrument-type instrument-type) stream)
  (print-unreadable-object (instrument-type stream :type t)
    (format stream "~A" (instrument-type-name instrument-type))))

(defun row->instrument-type (row)
  "Build an INSTRUMENT-TYPE from a row of (id name description)."
  (make-instance 'instrument-type
                 :id (first row)
                 :name (second row)
                 :description (from-db (third row))))

(defparameter *standard-instrument-types*
  '(("stock" "Equity listed on an exchange")
    ("bond" "Debt instrument")
    ("commodity" "Fungible physical or futures-traded good")
    ("currency" "Fiat currency")
    ("cryptocurrency" "Native on-chain asset")
    ("index" "Published basket or benchmark, not itself traded")
    ("fund" "Pooled vehicle (mutual fund, ETF, ...)"))
  "The conventional instrument types, as (name description) pairs.")

(defun standard-instrument-types ()
  "The conventional (name description) instrument-type pairs.  A fresh copy
   is returned, so callers may mutate the result freely."
  (mapcar #'(lambda (d) (copy-list d)) *standard-instrument-types*))

(defun make-instrument-type (name &key description)
  "Find or create the INSTRUMENT-TYPE with the given NAME and return it.
   NAME is canonicalized (trimmed and down-cased).  DESCRIPTION, when given,
   fills in a missing description on an existing type."
  (let ((canonical (canonicalize-slug name)))
    (let ((row (postmodern:query
                "insert into instrument_types (name, description)
                 values ($1, $2)
                 on conflict (name) do update
                   set description = coalesce(instrument_types.description,
                                              excluded.description)
                 returning id, name, description"
                canonical (to-db description) :row)))
      (row->instrument-type row))))

(defun instrument-type-by-name (name)
  "The INSTRUMENT-TYPE whose canonical name is NAME, or NIL."
  (let ((row (first-row
              "select id, name, description from instrument_types
               where name = $1 limit 1"
              (canonicalize-slug name))))
    (and row (row->instrument-type row))))

(defun instrument-type-id-for (type)
  "The database UUID of TYPE, or NIL when TYPE is NIL.  TYPE may be an
   INSTRUMENT-TYPE object or a name string (which is found or created)."
  (cond ((null type) nil)
        ((instrument-type-p type)
         (instrument-type-id type))
        ((stringp type)
         (instrument-type-id (make-instrument-type type)))
        (t (error "Cannot resolve the instrument type ~S" type))))

(defun all-instrument-types ()
  "All instrument types, alphabetically by name."
  (mapcar #'row->instrument-type
          (postmodern:query
           "select id, name, description from instrument_types order by name"
           :rows)))

(defun ensure-standard-instrument-types ()
  "Insert the standard stock/bond/commodity/currency/cryptocurrency/index/fund
   types if they are not already present, and return them."
  (mapcar (lambda (d)
            (make-instrument-type (first d) :description (second d)))
          *standard-instrument-types*))

(behavior 'standard-instrument-types
  (should= 7 (length *standard-instrument-types*))
  (should-equal '("stock" "Equity listed on an exchange")
                (first *standard-instrument-types*))
  (should-equal '("cryptocurrency" "Native on-chain asset")
                (fifth *standard-instrument-types*))
  (let ((copy (standard-instrument-types)))
    (setf (first (first copy)) "mutated")
    (should-string= "stock" (first (first *standard-instrument-types*)))))

(behavior 'instrument-type
  (let ((type (make-instance 'instrument-type
                             :id "t1" :name "cryptocurrency"
                             :description "Native on-chain asset")))
    (should-be-a 'instrument-type type)
    (should-be-true (instrument-type-p type))
    (should-string= "t1" (instrument-type-id type))
    (should-string= "cryptocurrency" (instrument-type-name type))
    (should-string= "Native on-chain asset"
                    (instrument-type-description type))
    (should-string= "#<INSTRUMENT-TYPE cryptocurrency>"
                    (with-output-to-string (s) (princ type s)))))
