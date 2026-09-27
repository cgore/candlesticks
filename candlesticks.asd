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

(defpackage :candlesticks/system
  (:use :common-lisp
        :asdf)
  (:export :version-string
           :version-list
           :version-major
           :version-minor
           :version-revision))
(in-package :candlesticks/system)

(defparameter version-major 0)
(defparameter version-minor 2)
(defparameter version-revision 1)

(defun version-list ()
  (list version-major version-minor version-revision))

(defun version-string ()
  (format nil "~{~A.~A.~A~}" (version-list)))

(defsystem "candlesticks"
  :description "Candlesticks is an interface to a PostgreSQL database of
           historical, time-series financial data: OHLC candlesticks for any
           fungible, traded instrument -- stocks, bonds, cryptocurrencies,
           commodities, and so on -- together with the provenance of each
           bar, namely which data source it came from and the specific
           retrieval that produced it."
  :version #.(version-string)
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "BSD-3-Clause"
  :homepage "https://github.com/cgore/candlesticks"
  :source-control (:git "https://github.com/cgore/candlesticks.git")
  :bug-tracker "https://github.com/cgore/candlesticks/issues"
  :depends-on ("sigma"
               "postmodern"
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.postmodern-postgresql")

  ;; Specs live in the sources as BEHAVIOR/SHOULD forms and run at load time.
  ;; TEST-OP reloads every source file so those top-level assertions run again.
  ;; (We LOAD sources directly rather than LOAD-SYSTEM :FORCE T, which ASDF
  ;; forbids inside a nested OPERATE.)  Set CANDLESTICKS_LIVE_TESTS=1 to also
  ;; run the live database behaviors in source/live-behaviors.lisp.
  :in-order-to ((test-op (load-op "candlesticks")))
  :perform (test-op (operation system)
                    (declare (ignore operation))
                    (labels ((reload (component)
                               (typecase component
                                 (cl-source-file
                                  (load (component-pathname component)))
                                 (parent-component
                                  (map nil #'reload (component-children component))))))
                      (reload system)
                      (when (uiop:getenv "CANDLESTICKS_LIVE_TESTS")
                        (load (system-relative-pathname system
                                                        "source/live-behaviors.lisp")))))

  :components ((:module "source"
                :components ((:file "config"
                              :depends-on ())
                             (:file "durations"
                              :depends-on ("config"))
                             (:file "instrument-types"
                              :depends-on ("config"))
                             (:file "instruments"
                              :depends-on ("config"
                                           "instrument-types"))
                             (:file "data-source-kinds"
                              :depends-on ("config"))
                             (:file "data-sources"
                              :depends-on ("config"
                                           "data-source-kinds"))
                             (:file "exchanges"
                              :depends-on ("config"))
                             (:file "data-retrievals"
                              :depends-on ("config"
                                           "data-sources"
                                           "exchanges"))
                             (:file "tickers"
                              :depends-on ("config"
                                           "instruments"
                                           "data-sources"
                                           "exchanges"))
                             (:file "bars"
                              :depends-on ("config"
                                           "durations"
                                           "instruments"
                                           "data-sources"
                                           "exchanges"
                                           "tickers"
                                           "data-retrievals"))
                             (:file "migrations"
                              :depends-on ("config"))
                             (:file "candlesticks"
                              :depends-on ("config"
                                           "durations"
                                           "instrument-types"
                                           "instruments"
                                           "data-source-kinds"
                                           "data-sources"
                                           "exchanges"
                                           "tickers"
                                           "data-retrievals"
                                           "bars"
                                           "migrations"))))))
