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

;;; The Candlesticks umbrella package.  It re-exports every public symbol
;;; from the sub-packages, so that (use-package :candlesticks) --- or
;;; (in-package :candlesticks) --- gives you the whole library at once.
;;; :REEXPORT is UIOP's "use these packages and re-export their exports",
;;; so this list does not have to stay in sync with every new accessor.

(uiop:define-package :candlesticks
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :candlesticks/durations
        :candlesticks/instrument-types
        :candlesticks/instruments
        :candlesticks/data-source-kinds
        :candlesticks/data-sources
        :candlesticks/exchanges
        :candlesticks/tickers
        :candlesticks/data-retrievals
        :candlesticks/bars
        :candlesticks/migrations
        :candlesticks/system)
  (:reexport :candlesticks/config
             :candlesticks/durations
             :candlesticks/instrument-types
             :candlesticks/instruments
             :candlesticks/data-source-kinds
             :candlesticks/data-sources
             :candlesticks/exchanges
             :candlesticks/tickers
             :candlesticks/data-retrievals
             :candlesticks/bars
             :candlesticks/migrations
             :candlesticks/system))
(in-package :candlesticks)

(defun umbrella-exports-p (name)
  "True when NAME is an external symbol of the CANDLESTICKS package."
  (eq :external (nth-value 1 (find-symbol name (find-package :candlesticks)))))

(behavior 'version-string
  (should-equal '(0 2 1) (version-list))
  (should-string= "0.2.1" (version-string)))

(behavior 'use-all-symbols
  (should-be-true (umbrella-exports-p "CANDLESTICK"))
  (should-be-true (umbrella-exports-p "INGEST-CANDLESTICKS"))
  (should-be-true (umbrella-exports-p "WITH-CONNECTION"))
  (should-be-true (umbrella-exports-p "RUN-MIGRATIONS"))
  (should-be-true (umbrella-exports-p "INSTRUMENTP"))
  (should-be-true (umbrella-exports-p "DURATIONP"))
  (should-be-true (umbrella-exports-p "DATA-SOURCE-ID-FOR"))
  (should-be-true (umbrella-exports-p "DATA-RETRIEVAL-SOURCE-NAME"))
  (should-be-true (umbrella-exports-p "INSTRUMENT-TYPE"))
  (should-be-true (umbrella-exports-p "DATA-SOURCE-KIND"))
  (should-be-true (umbrella-exports-p "ENSURE-STANDARD-INSTRUMENT-TYPES"))
  (should-be-true (umbrella-exports-p "ENSURE-STANDARD-DATA-SOURCE-KINDS"))
  (should-be-true (umbrella-exports-p "TICKER"))
  (should-be-true (umbrella-exports-p "ENSURE-INSTRUMENT-FOR-TICKER"))
  (should-be-true (umbrella-exports-p "EXCHANGE"))
  (should-be-true (umbrella-exports-p "ENSURE-STANDARD-EXCHANGES"))
  (should-be-true (umbrella-exports-p "VERSION-STRING"))
  ;; sigma/behave is :USE'd for specs in this file, not re-exported.
  (should-be-false (umbrella-exports-p "SHOULD=")))
