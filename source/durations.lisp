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

(defpackage :candlesticks/durations
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config)
  (:export :duration
           :duration-id
           :duration-short-name
           :duration-long-name
           :duration-interval
           :make-duration
           :duration-by-short-name
           :duration-by-long-name
           :all-durations
           :standard-durations
           :ensure-standard-durations
           :durationp))
(in-package :candlesticks/durations)

(defclass duration ()
  ((id
    :initarg :id
    :accessor duration-id
    :type string
    :initform ""
    :documentation "The UUID of this duration.")
   (short-name
    :initarg :short-name
    :accessor duration-short-name
    :type string
    :initform ""
    :documentation "The short name, e.g. \"w\" for a week.")
   (long-name
    :initarg :long-name
    :accessor duration-long-name
    :type string
    :initform ""
    :documentation "The long name, e.g. \"week\".")
   (interval
    :initarg :interval
    :accessor duration-interval
    :initform nil
    :documentation "The PostgreSQL interval, as Postmodern returns it."))
  (:documentation
   "One candlestick duration: a short name, a long name, and the PostgreSQL
   interval that corresponds to it."))

(defun durationp (object)
  "True if OBJECT is a DURATION."
  (typep object 'duration))

(defmethod print-object ((duration duration) stream)
  (print-unreadable-object (duration stream :type t)
    (format stream "~A / ~A" (duration-short-name duration)
            (duration-long-name duration))))

(defun row->duration (row)
  "Build a DURATION from a row of (id short-name long-name interval)."
  (make-instance 'duration
                 :id (first row)
                 :short-name (second row)
                 :long-name (third row)
                 :interval (fourth row)))

(defparameter *standard-durations*
  '(("s" "second" "1 second")
    ("m" "minute" "1 minute")
    ("h" "hour" "1 hour")
    ("d" "day" "1 day")
    ("w" "week" "1 week")
    ("M" "month" "1 month")
    ("y" "year" "1 year"))
  "The conventional durations, as (short-name long-name interval) triples.")

(defun standard-durations ()
  "The conventional (short-name long-name interval) duration triples.  A fresh
   copy is returned, so callers may mutate the result freely."
  (mapcar #'(lambda (d) (copy-list d)) *standard-durations*))

(defun make-duration (short-name long-name interval)
  "Ensure a DURATION with SHORT-NAME, LONG-NAME, and a PostgreSQL INTERVAL
   exists, and return it.  Short names are unique on their own, and so are
   long names: there is only one \"d\" and only one \"week\".  If the short
   name and long name already belong to two different rows, that is an
   error."
  (let ((by-short (duration-by-short-name short-name))
        (by-long (duration-by-long-name long-name)))
    (when (and by-short by-long
               (string/= (duration-id by-short) (duration-id by-long)))
      (error "Duration short name ~S and long name ~S belong to different rows."
             short-name long-name))
    (let ((id (cond (by-short (duration-id by-short))
                    (by-long (duration-id by-long))
                    (t nil))))
      (if id
          (progn
            (postmodern:execute
             "update candlestick_durations
              set short_name = $2, long_name = $3, duration = $4
              where id = $1"
             id short-name long-name interval)
            (duration-by-short-name short-name))
          (row->duration
           (postmodern:query
            "insert into candlestick_durations (short_name, long_name, duration)
             values ($1, $2, $3)
             returning id, short_name, long_name, duration"
            short-name long-name interval :row))))))

(defun duration-by-short-name (short-name)
  "The DURATION with the given short name, or NIL."
  (let ((row (first-row
              "select id, short_name, long_name, duration
               from candlestick_durations where short_name = $1 limit 1"
              short-name)))
    (and row (row->duration row))))

(defun duration-by-long-name (long-name)
  "The DURATION with the given long name, or NIL."
  (let ((row (first-row
              "select id, short_name, long_name, duration
               from candlestick_durations where long_name = $1 limit 1"
              long-name)))
    (and row (row->duration row))))

(defun all-durations ()
  "All durations, in short-name order."
  (mapcar #'row->duration
          (postmodern:query
           "select id, short_name, long_name, duration
            from candlestick_durations order by short_name"
           :rows)))

(defun ensure-standard-durations ()
  "Insert the standard second/minute/hour/day/week/month/year durations if
   they are not already present, and return them."
  (mapcar (lambda (d) (apply #'make-duration d)) *standard-durations*))

(behavior 'standard-durations
  (should= 7 (length *standard-durations*))
  (should-equal '("s" "second" "1 second") (first *standard-durations*))
  (should-equal '("w" "week" "1 week") (fifth *standard-durations*))
  (should-be-true
   (every #'(lambda (d) (= 3 (length d))) *standard-durations*))
  ;; standard-durations hands out copies, so mutating the result is safe.
  (let ((copy (standard-durations)))
    (setf (first (first copy)) "mutated")
    (should-string= "s" (first (first *standard-durations*)))))

(behavior 'duration
  (let ((d (make-instance 'duration
                          :id "abc" :short-name "w" :long-name "week"
                          :interval nil)))
    (should-be-a 'duration d)
    (should-string= "abc" (duration-id d))
    (should-string= "w" (duration-short-name d))
    (should-string= "week" (duration-long-name d))
    (should-string= "#<DURATION w / week>"
                    (with-output-to-string (s) (princ d s)))))
