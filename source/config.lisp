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

(defpackage :candlesticks/config
  (:use :common-lisp
        :sigma/behave)
  (:export :*default-connection-spec*
           :default-connection-spec
           :set-default-connection-spec!
           :connection-spec
           :current-connection
           :connected-p
           :require-connection!
           :with-connection
           :universal-time->timestamptz
           :canonicalize-slug
           :to-db
           :from-db
           :coerce-price
           :first-row))
(in-package :candlesticks/config)

;;;;
;;;; Connections
;;;;

(defparameter *default-connection-spec* nil
  "A Postmodern connection spec (a list that POSTMODERN:CONNECT can be applied
   to) used by WITH-CONNECTION when no explicit spec is supplied.")

(defun default-connection-spec ()
  "The current *default-connection-spec*."
  *default-connection-spec*)

(defun set-default-connection-spec! (spec-list)
  "Set *default-connection-spec* to SPEC-LIST and return SPEC-LIST."
  (setf *default-connection-spec* spec-list))

(defun connection-spec (&optional (spec-list *default-connection-spec*))
  "Return SPEC-LIST, or *default-connection-spec* when SPEC-LIST is NIL.
   Errors when neither is available.  A spec is a list POSTMODERN:CONNECT can
   be applied to, e.g. (\"database\" \"user\" \"password\" \"host\" :port 5432)."
  (or spec-list
      (error "No connection spec supplied and *default-connection-spec* is unset. Pass a spec to WITH-CONNECTION, or set *default-connection-spec* first. A spec is a list POSTMODERN:CONNECT can be applied to, e.g. (\"database\" \"user\" \"password\" \"host\" :port 5432)")))

(defun current-connection ()
  "The active Postmodern connection, or NIL when none is bound."
  postmodern:*database*)

(defun connected-p (&optional (connection (current-connection)))
  "True if CONNECTION (default: the active connection) is open and connected."
  (and connection (postmodern:connected-p connection)))

(defun require-connection! ()
  "Return the active Postmodern connection, erroring clearly if there is none."
  (or (current-connection)
      (error "No active database connection. Run inside WITH-CONNECTION, or set *default-connection-spec* and use WITH-CONNECTION.")))

(defmacro with-connection (spec-list &body body)
  "Evaluate BODY with the active database connection bound to a connection
   opened from SPEC-LIST.  SPEC-LIST is an expression evaluating to a
   Postmodern connection spec -- a list that POSTMODERN:CONNECT can be applied
   to, e.g.

     (\"database\" \"user\" \"password\" \"host\" :port 5432)

   When SPEC-LIST is NIL, *default-connection-spec* is used instead.  The
   connection is closed when BODY finishes.  Nesting or repeating the form
   gives you several independent connections, which is how you work against
   more than one database at a time."
  `(postmodern:call-with-connection (connection-spec ,spec-list)
     #'(lambda () ,@body)))

(behavior 'connection-spec
  (let ((signaled nil))
    (handler-case (connection-spec)
      (error () (setf signaled t)))
    (should-be-true signaled))
  (let ((*default-connection-spec* '("db" "user" "pw" "host" :port 5432)))
    (should-equalp '("db" "user" "pw" "host" :port 5432)
                   (connection-spec)))
  (let ((*default-connection-spec* '("a" "b" "c" "d")))
    ;; An explicit spec wins over the default.
    (should-equalp '("x" "y" "z" "w")
                   (connection-spec '("x" "y" "z" "w")))))

(behavior 'with-connection
  ;; WITH-CONNECTION expands into POSTMODERN:CALL-WITH-CONNECTION, binding
  ;; the active connection for the body.  The spec is resolved at run time
  ;; by CONNECTION-SPEC, which is where a missing spec becomes an error.
  (let ((expanded (macroexpand-1 `(with-connection spec-list (body)))))
    (should-string= "CALL-WITH-CONNECTION" (symbol-name (first expanded)))
    (should-equalp '(connection-spec spec-list) (second expanded))
    ;; The body is wrapped in a lambda object (or closure) for the thunk.
    (should-be-true (or (listp (third expanded))
                        (functionp (third expanded))))))

;;;;
;;;; Shared value conversions
;;;;

(defun universal-time->timestamptz (universal-time)
  "Format UNIVERSAL-TIME (seconds since 1900-01-01 UTC) as a PostgreSQL
   'timestamp with time zone' literal in UTC, e.g. \"2024-01-15 12:30:45+00\".
   This is the form Postmodern/PostgreSQL accept for a timestamptz parameter.
   Note that when such a value is read back, Postmodern hands it to us as a
   universal time again."
  (multiple-value-bind (sec min hour day month year dst zone)
      (decode-universal-time universal-time 0)
    (declare (ignore dst zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D+00"
            year month day hour min sec)))

(behavior 'universal-time->timestamptz
  (let ((epoch (encode-universal-time 0 0 0 1 1 1970 0)))
    (should-string= "1970-01-01 00:00:00+00" (universal-time->timestamptz epoch)))
  (should-string= "2026-08-18 12:00:00+00"
                  (universal-time->timestamptz
                   (encode-universal-time 0 0 12 18 8 2026 0))))

(defun canonicalize-slug (name)
  "Trim leading and trailing whitespace from NAME and down-case it, so that
   \"API\", \" api \" and \"Api\" are the same slug.  Used for instrument
   type names and data source kind names."
  (string-downcase
   (string-trim '(#\Space #\Tab #\Return #\Newline) (string name))))

(behavior 'canonicalize-slug
  (should-string= "cryptocurrency" (canonicalize-slug "cryptocurrency"))
  (should-string= "cryptocurrency" (canonicalize-slug "Cryptocurrency"))
  (should-string= "api" (canonicalize-slug "  API  "))
  (should-string= "stock" (canonicalize-slug "Stock")))

(defun to-db (value)
  "Convert a Lisp NIL into the Postmodern NULL marker :null, leaving any
   other VALUE unchanged."
  (if (null value)
      :null
      value))

(defun from-db (value)
  "Convert a Postmodern NULL marker :null into Lisp NIL, leaving any other
   VALUE unchanged."
  (if (eq value :null)
      nil
      value))

(defun coerce-price (value)
  "Coerce a numeric VALUE read back from the database (Postmodern returns
   NUMERIC columns as rationals) into a FLOAT.  NIL and :null pass through
   as NIL, so nullable columns like volume stay NIL."
  (if (or (null value) (eq value :null))
    nil
    (float value)))

(behavior 'to-db
  (should-eq :null (to-db nil))
  (should= 12 (to-db 12))
  (should-string= "x" (to-db "x")))

(behavior 'from-db
  (should-be-null (from-db :null))
  (should-be-null (from-db nil))
  (should= 12 (from-db 12)))

(behavior 'coerce-price
  (should= 123.4567 (coerce-price 1234567/10000))
  (should= 42.0 (coerce-price 42))
  (should= 9876.5 (coerce-price 19753/2))
  (should-be-null (coerce-price nil))
  (should-be-null (coerce-price :null)))

(defmacro first-row (sql &rest params)
  "Run the parameterized SQL and return the first row as a list of values, or
   NIL when there is no row.  PARAMS are the values bound to the $1, $2, ...
   placeholders, in order.  This is a macro so that its parameters are
   compile-time forms, which is what Postmodern's query macro requires."
  `(let ((rows (postmodern:query ,sql :rows ,@params)))
     (first rows)))
