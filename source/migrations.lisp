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

(defpackage :candlesticks/migrations
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :migratum
        :migratum.provider.local-path
        :migratum.driver.postmodern-postgresql)
  (:export :migrations-path
           :connection-spec->migratum-spec
           :with-migration-driver
           :run-migrations
           :revert-last-migration
           :pending-migrations
           :applied-migrations
           :latest-migration-id
           :migration-status?))
(in-package :candlesticks/migrations)

(defun migrations-path ()
  "The directory holding the Candlesticks migration files."
  (asdf:system-relative-pathname :candlesticks "migrations/"))

(defun connection-spec->migratum-spec (spec-list)
  "Convert a Postmodern connection spec (a list that POSTMODERN:CONNECT can be
   applied to) into the keyword spec the cl-migratum Postmodern driver
   expects."
  (destructuring-bind (database user password host &rest keywords) spec-list
    (append (list :database database :user-name user :password password
                  :host host)
            keywords)))

(behavior 'connection-spec->migratum-spec
  (should-equalp '(:database "db" :user-name "u" :password "pw"
                     :host "localhost")
                 (connection-spec->migratum-spec '("db" "u" "pw" "localhost")))
  (should-equalp '(:database "db" :user-name "u" :password "pw"
                     :host "h" :port 5432)
                 (connection-spec->migratum-spec '("db" "u" "pw" "h" :port 5432))))

(defmacro with-migration-driver ((driver-var &optional spec-list schema) &body body)
  "Create a cl-migratum driver -- which opens Postmodern's top-level
   connection -- for SPEC-LIST, optionally SET the connection's search_path to
   SCHEMA, initialize the driver, run BODY with it bound to DRIVER-VAR, and
   disconnect the top-level connection when BODY finishes (whether normally or
   by error).

   SPEC-LIST is a Postmodern connection spec, or NIL for
   *default-connection-spec*.  The cl-migratum Postmodern driver takes
   ownership of the top-level connection, so this form always ends with it
   disconnected."
  (let ((spec-var (gensym)) (drv (gensym)))
    `(let* ((,spec-var (connection-spec ,spec-list))
            (,drv (migratum.driver.postmodern-postgresql:make-driver
                   (make-migration-provider)
                   (connection-spec->migratum-spec ,spec-var))))
       (unwind-protect
            (progn
              (when ,schema
                (postmodern:execute
                 (format nil "SET search_path TO ~A" ,schema)))
              (migratum:driver-init ,drv)
              (let ((,driver-var ,drv)) ,@body))
          (postmodern:disconnect-toplevel)))))

(defun make-migration-provider ()
  "A cl-migratum local-path provider that discovers the Candlesticks
   migrations from MIGRATIONS-PATH."
  (migratum.provider.local-path:make-provider (list (migrations-path))))

(defun run-migrations (spec-list &optional schema)
  "Apply all pending Candlesticks migrations to the database described by SPEC.
   When SCHEMA is given, it is first set as the connection's search_path, so
   the tables are created in that schema.  Returns the list of applied
   migrations."
  (with-migration-driver (driver spec-list schema)
    (migratum:apply-pending driver)))

(defun revert-last-migration (spec-list &optional schema)
  "Revert the most recently applied Candlesticks migration.  Returns the
   reverted migration."
  (with-migration-driver (driver spec-list schema)
    (migratum:revert-last driver)))

(defun pending-migrations (spec-list &optional schema)
  "The Candlesticks migrations that have not yet been applied."
  (with-migration-driver (driver spec-list schema)
    (migratum:list-pending driver)))

(defun applied-migrations (spec-list &optional schema)
  "The Candlesticks migrations that have already been applied."
  (with-migration-driver (driver spec-list schema)
    (migratum:driver-list-applied driver)))

(defun latest-migration-id (spec-list &optional schema)
  "The id of the most recently applied Candlesticks migration, or NIL."
  (with-migration-driver (driver spec-list schema)
    (when (migratum:contains-applied-migrations-p driver)
      (migratum:migration-id (migratum:latest-migration driver)))))

(defun migration-status? (spec-list &optional schema)
  "True if at least one Candlesticks migration has been applied to the given
   database (in SCHEMA, when given)."
  (with-migration-driver (driver spec-list schema)
    (migratum:contains-applied-migrations-p driver)))
