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

(defpackage :candlesticks/instruments
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :candlesticks/instrument-types)
  (:export :instrument
           :instrument-id
           :instrument-name
           :instrument-type
           :instrument-type-id
           :instrument-type-name
           :make-instrument
           :fill-instrument
           :instrument-by-id
           :all-instruments
           :instrumentp))
(in-package :candlesticks/instruments)

(defclass instrument ()
  ((id
    :initarg :id
    :accessor instrument-id
    :type string
    :initform ""
    :documentation "The UUID of this instrument.  This is the identity;
   short tickers live on the TICKERS table and are not unique.")
   (name
    :initarg :name
    :accessor instrument-name
    :initform nil
    :documentation "A human readable name, e.g. \"Bitcoin\".  Not unique.")
   (instrument-type-id
    :initarg :instrument-type-id
    :accessor instrument-type-id
    :initform nil
    :documentation "The UUID of this instrument's type, or NIL.")
   (instrument-type
    :initarg :instrument-type
    :accessor instrument-type
    :initform nil
    :documentation "The INSTRUMENT-TYPE of this instrument, or NIL."))
  (:documentation
   "A single tradable instrument: a stock, bond, cryptocurrency, commodity,
   currency, index, or anything else that exists in a fungible traded
   market.  Identity is the UUID.  Market abbreviations are TICKERS, and
   the same abbreviation can mean different instruments at different
   sources."))

(defun instrumentp (object)
  "True if OBJECT is an INSTRUMENT."
  (typep object 'instrument))

(defmethod print-object ((instrument instrument) stream)
  (print-unreadable-object (instrument stream :type t)
    (format stream "~A" (or (instrument-name instrument)
                            (instrument-id instrument)))))

(defmethod instrument-type-name ((instrument instrument))
  "The canonical name of INSTRUMENT's type, or NIL."
  (let ((type (instrument-type instrument)))
    (and type (instrument-type-name type))))

(defparameter *instrument-select*
  "select i.id, i.name, t.id, t.name, t.description
   from instruments i
   left join instrument_types t on t.id = i.instrument_type_id"
  "The SELECT used to load an instrument together with its type.")

(defun row->instrument (row)
  "Build an INSTRUMENT from a joined row of
   (id name type-id type-name type-description)."
  (let* ((type-id (from-db (third row)))
         (type (and type-id
                    (make-instance 'instrument-type
                                   :id type-id
                                   :name (fourth row)
                                   :description (from-db (fifth row))))))
    (make-instance 'instrument
                   :id (first row)
                   :name (from-db (second row))
                   :instrument-type-id type-id
                   :instrument-type type)))

(defun instrument-by-id (id)
  "The INSTRUMENT with the given UUID, or NIL."
  (let ((row (first-row
              (concatenate 'string *instrument-select* " where i.id = $1")
              id)))
    (and row (row->instrument row))))

(defun make-instrument (&key name instrument-type)
  "Create a new INSTRUMENT and return it.  Identity is the UUID; short
   market abbreviations are attached separately as TICKERS.  NAME (e.g.
   \"Bitcoin\") and INSTRUMENT-TYPE (an INSTRUMENT-TYPE or a name such as
   \"cryptocurrency\") are optional."
  (let ((type-id (instrument-type-id-for instrument-type)))
    (let ((id (postmodern:query
               "insert into instruments (name, instrument_type_id)
                values ($1, $2)
                returning id"
               (to-db name) (to-db type-id)
               :single)))
      (instrument-by-id id))))

(defun fill-instrument (instrument &key name instrument-type)
  "Fill in any missing NAME or INSTRUMENT-TYPE on INSTRUMENT, without
   overwriting values that are already set.  Returns the (possibly updated)
   instrument."
  (let ((type-id (instrument-type-id-for instrument-type)))
    (postmodern:execute
     "update instruments
      set name = coalesce(instruments.name, $2),
          instrument_type_id = coalesce(instruments.instrument_type_id, $3),
          updated_at = now()
      where id = $1"
     (instrument-id instrument) (to-db name) (to-db type-id))
    (instrument-by-id (instrument-id instrument))))

(defun all-instruments ()
  "All instruments, alphabetically by name (unnamed last)."
  (mapcar #'row->instrument
          (postmodern:query
           (concatenate 'string *instrument-select*
                        " order by i.name nulls last, i.id")
           :rows)))

(behavior 'instrument
  (let* ((type (make-instance 'instrument-type
                              :id "t1" :name "cryptocurrency"))
         (inst (make-instance 'instrument
                              :id "id1" :name "Bitcoin"
                              :instrument-type-id "t1"
                              :instrument-type type)))
    (should-be-a 'instrument inst)
    (should-string= "id1" (instrument-id inst))
    (should-string= "Bitcoin" (instrument-name inst))
    (should-string= "t1" (instrument-type-id inst))
    (should-string= "cryptocurrency" (instrument-type-name inst))))
