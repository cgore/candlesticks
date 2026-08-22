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

;;; Live database behaviors for Candlesticks.
;;;
;;; These run against a real PostgreSQL server and are loaded only when the
;;; CANDLESTICKS_LIVE_TESTS environment variable is set (see the test-op in
;;; candlesticks.asd).  They run the migrations into a throwaway schema,
;;; exercise the whole API against it -- including provenance, duplicate
;;; ingestion, and several simultaneous connections -- and drop the schema
;;; when they are done.

(in-package :candlesticks)

;;;;
;;;; Test connection and schema
;;;;

(defparameter *live-schema* "candlesticks_test"
  "The PostgreSQL schema the live tests run in.  It is dropped and
   re-created for every test run, so the tests are safe to repeat.")

(defun live-spec ()
  "The Postmodern connection spec for the live tests.  The
   CANDLESTICKS_LIVE_DB environment variable must be set to a
   whitespace-separated list of database, user, password, host, and port,
   e.g. \"candlesticks candlesticks secret localhost 5432\"."
  (let ((env (uiop:getenv "CANDLESTICKS_LIVE_DB")))
    (unless env
      (error "CANDLESTICKS_LIVE_DB is unset. It must be a whitespace-separated list of database, user, password, host, and port."))
    (let ((parts (remove "" (uiop:split-string env) :test #'string=)))
      (unless (= 5 (length parts))
        (error "CANDLESTICKS_LIVE_DB must have five fields: database user password host port."))
      (list (first parts) (second parts) (third parts)
            (fourth parts) :port (parse-integer (fifth parts))))))

(defun reset-live-schema (spec-list)
  "Drop and re-create the live test schema so a test run starts from a clean
   slate."
  (with-connection spec-list
    (postmodern:execute (format nil "drop schema if exists ~A cascade"
                                *live-schema*))
    (postmodern:execute (format nil "create schema ~A" *live-schema*))))

(defmacro with-live-schema (&body body)
  "Evaluate BODY on a connection to the live test database, with the search
   path set to *live-schema*."
  `(with-connection (live-spec)
     (postmodern:execute (format nil "set search_path to ~A" *live-schema*))
     ,@body))

(defun schema-table-p (table-name)
  "True if a table called TABLE-NAME exists in the live test schema."
  (let ((row (first-row
              "select 1 from information_schema.tables
               where table_schema = current_schema() and table_name = $1"
              table-name)))
    (not (null row))))

(defun test-time (days)
  "2026-08-18 12:00:00 UTC plus DAYS whole days, as a universal time."
  (+ (encode-universal-time 0 0 12 18 8 2026 0)
     (* days 86400)))

;;;;
;;;; Migrations
;;;;

(behavior 'live-migrations
  (reset-live-schema (live-spec))
  (should-be-false (with-live-schema () (schema-table-p "candlesticks")))
  (run-migrations (live-spec) *live-schema*)
  (should-be-true (migration-status? (live-spec) *live-schema*))
  (with-live-schema ()
    (should-be-true (schema-table-p "instrument_types"))
    (should-be-true (schema-table-p "data_source_kinds"))
    (should-be-true (schema-table-p "instruments"))
    (should-be-true (schema-table-p "data_sources"))
    (should-be-true (schema-table-p "exchanges"))
    (should-be-true (schema-table-p "tickers"))
    (should-be-true (schema-table-p "candlestick_durations"))
    (should-be-true (schema-table-p "data_retrievals"))
    (should-be-true (schema-table-p "candlesticks")))
  ;; Running the migrations again is a no-op.
  (run-migrations (live-spec) *live-schema*)
  (should-be-true (migration-status? (live-spec) *live-schema*)))

(behavior 'live-revert
  (revert-last-migration (live-spec) *live-schema*))

(behavior 'live-re-apply
  (with-live-schema ()
    (should-be-false (schema-table-p "candlesticks")))
  (run-migrations (live-spec) *live-schema*)
  (with-live-schema ()
    (should-be-true (schema-table-p "candlesticks"))))

;;;;
;;;; Durations
;;;;

(behavior 'live-durations
  (with-live-schema ()
    (let ((durations (ensure-standard-durations)))
      (should= 7 (length durations))
      (should-be-a 'duration (first durations))
      (should-be-true
       (every #'(lambda (d) (and (duration-id d)
                                 (duration-short-name d)
                                 (duration-long-name d)
                                 (duration-interval d)))
              durations)))
    (let ((week (duration-by-short-name "w")))
      (should-string= "week" (duration-long-name week)))
    (should-string= "w" (duration-short-name
                         (duration-by-long-name "week")))
    (should= 7 (length (all-durations)))
    (should-be-null (duration-by-short-name "x"))
    ;; Each name is unique on its own.  Re-ensuring does not duplicate.
    (ensure-standard-durations)
    (should= 7 (length (all-durations)))
    (let ((day (duration-by-short-name "d")))
      (should-string= (duration-id day)
                      (duration-id (make-duration "d" "day" "1 day"))))
    ;; Mixing a short name from one row with a long name from another is
    ;; rejected.
    (let ((signaled nil))
      (handler-case (make-duration "d" "week" "1 day")
        (error () (setf signaled t)))
      (should-be-true signaled))))

;;;;
;;;; Exchanges
;;;;

(behavior 'live-exchanges
  (with-live-schema ()
    (let ((exchanges (ensure-standard-exchanges)))
      (should= 13 (length exchanges))
      (should-be-a 'exchange (first exchanges)))
    (let ((uni (exchange-by-name "Uniswap-V3-Ethereum")))
      (should-string= "uniswap-v3-ethereum" (exchange-name uni))
      (should-string= "ethereum" (exchange-chain uni))
      (should-string= "XNAS" (exchange-mic (exchange-by-name "nasdaq"))))
    (should-be-null (exchange-by-name "no-such-exchange"))))

;;;;
;;;; Instrument types
;;;;

(behavior 'live-instrument-types
  (with-live-schema ()
    (let ((types (ensure-standard-instrument-types)))
      (should= 7 (length types))
      (should-be-a 'instrument-type (first types)))
    (let ((crypto (instrument-type-by-name "Cryptocurrency")))
      (should-string= "cryptocurrency" (instrument-type-name crypto))
      (should-string= "Native on-chain asset"
                      (instrument-type-description crypto)))
    (should= 7 (length (all-instrument-types)))
    (should-be-null (instrument-type-by-name "no-such-type"))))

;;;;
;;;; Instruments
;;;;

(behavior 'live-instruments
  (with-live-schema ()
    (let* ((btc (make-instrument :name "Bitcoin"
                                 :instrument-type "cryptocurrency"))
           (usd (make-instrument :name "US Dollar"
                                 :instrument-type "currency")))
      (should-be-a 'instrument btc)
      (should-be-true (instrument-id btc))
      (should-string= "Bitcoin" (instrument-name btc))
      (should-string= "cryptocurrency" (instrument-type-name btc))
      (should-be-true (instrument-id usd))
      (should-not-string= (instrument-id btc) (instrument-id usd))
      (should-string= "Bitcoin"
                      (instrument-name (instrument-by-id (instrument-id btc))))
      ;; fill-instrument fills missing fields, does not overwrite.
      (let ((again (fill-instrument btc :instrument-type "stock")))
        (should-string= (instrument-id btc) (instrument-id again))
        (should-string= "cryptocurrency" (instrument-type-name again))))
    (should= 2 (length (all-instruments)))))

;;;;
;;;; Data source kinds
;;;;

(behavior 'live-data-source-kinds
  (with-live-schema ()
    (let ((kinds (ensure-standard-data-source-kinds)))
      (should= 5 (length kinds))
      (should-be-a 'data-source-kind (first kinds)))
    (let ((api (data-source-kind-by-name "API")))
      (should-string= "api" (data-source-kind-name api))
      (should-string= "Programmatic HTTP/RPC API"
                      (data-source-kind-description api)))
    (should= 5 (length (all-data-source-kinds)))
    (should-be-null (data-source-kind-by-name "no-such-kind"))))

;;;;
;;;; Data sources
;;;;

(behavior 'live-data-sources
  (with-live-schema ()
    (let ((coingecko (make-data-source "CoinGecko" :kind "api")))
      (should-be-a 'data-source coingecko)
      (should-be-true (data-source-id coingecko))
      (should-string= "CoinGecko" (data-source-name coingecko))
      (should-string= "api" (data-source-kind-name coingecko))
      (should-string= (data-source-id coingecko)
                      (data-source-id (data-source-by-name "CoinGecko")))
      (should-be-null (data-source-by-name "No Such Source")))
    (let ((zapper (make-data-source "Zapper.fi" :kind "website")))
      (should-not-string= (data-source-id zapper)
                          (data-source-id (data-source-by-name "CoinGecko")))
      ;; Re-creating a source does not overwrite an existing kind.
      (let ((again (make-data-source "CoinGecko" :kind "website")))
        (should-string= "api" (data-source-kind-name again))))
    (should= 2 (length (all-data-sources)))))

;;;;
;;;; Tickers
;;;;

(behavior 'live-tickers
  (with-live-schema ()
    (let* ((btc (find-if (lambda (i) (string= "Bitcoin" (instrument-name i)))
                         (all-instruments)))
           (usd (find-if (lambda (i) (string= "US Dollar" (instrument-name i)))
                         (all-instruments)))
           (tk (make-ticker btc "btc" :source "CoinGecko")))
      (should-be-a 'ticker tk)
      (should-string= "BTC" (ticker-canonical tk))
      (should-string= "btc" (ticker-symbol tk))
      (should-string= (instrument-id btc) (ticker-instrument-id tk))
      ;; Same source + abbreviation is the same ticker, not a reassignment.
      (let ((again (make-ticker usd "BTC" :source "CoinGecko")))
        (should-string= (ticker-id tk) (ticker-id again))
        (should-string= (instrument-id btc) (ticker-instrument-id again)))
      (make-ticker usd "usd" :source "CoinGecko")
      (should-string= (instrument-id btc)
                      (instrument-id (instrument-for-ticker "BTC"
                                                           :source "CoinGecko")))
      (should-string= (instrument-id usd)
                      (instrument-id (instrument-for-ticker "usd"
                                                           :source "CoinGecko")))
      (should-be-null (ticker-valid-from tk))
      (should-be-null (ticker-valid-to tk)))))

(behavior 'live-ticker-validity
  (with-live-schema ()
    (let* ((old-gm (make-instrument :name "General Motors (old)"
                                    :instrument-type "stock"))
           (new-gm (make-instrument :name "General Motors (new)"
                                    :instrument-type "stock"))
           (split (encode-universal-time 0 0 0 10 7 2009 0))
           (before (encode-universal-time 0 0 0 1 1 2005 0))
           (after (encode-universal-time 0 0 0 1 1 2012 0))
           (old-tk (make-ticker old-gm "GM" :source "Yahoo Finance"
                                :valid-from nil :valid-to split
                                :at before)))
      (should-string= (instrument-id old-gm) (ticker-instrument-id old-tk))
      (should= split (ticker-valid-to old-tk))
      (should-string= (instrument-id old-gm)
                      (instrument-id
                       (instrument-for-ticker "GM" :source "Yahoo Finance"
                                              :at before)))
      (should-be-null (instrument-for-ticker "GM" :source "Yahoo Finance"
                                            :at after))
      ;; An overlapping window for the same abbreviation is rejected.
      (let ((failed nil))
        (handler-case
            (make-ticker new-gm "GM" :source "Yahoo Finance"
                         :valid-from (encode-universal-time 0 0 0 1 1 2008 0)
                         :valid-to (encode-universal-time 0 0 0 1 1 2011 0)
                         :at (encode-universal-time 0 0 0 1 1 2010 0))
          (error () (setf failed t)))
        (should-be-true failed))
      (let ((new-tk (make-ticker new-gm "GM" :source "Yahoo Finance"
                                 :valid-from split :at after)))
        (should-not-string= (ticker-id old-tk) (ticker-id new-tk))
        (should-string= (instrument-id new-gm) (ticker-instrument-id new-tk))
        (should-string= (instrument-id old-gm)
                        (instrument-id
                         (instrument-for-ticker "GM" :source "Yahoo Finance"
                                                :at before)))
        (should-string= (instrument-id new-gm)
                        (instrument-id
                         (instrument-for-ticker "GM" :source "Yahoo Finance"
                                                :at after)))
        (should= 2 (length (tickers-for-symbol "GM"))))
      (let* ((open-tk (make-ticker old-gm "DELPHI" :source "Yahoo Finance"))
             (closed (close-ticker open-tk split)))
        (should= split (ticker-valid-to closed))
        (should-be-null (ticker-by-source-and-symbol "Yahoo Finance" "DELPHI"
                                                     :at after))
        (let ((reborn (make-ticker new-gm "DELPHI" :source "Yahoo Finance"
                                   :valid-from split :at after)))
          (should-not-string= (ticker-id open-tk) (ticker-id reborn))
          (should-string= (instrument-id new-gm)
                          (ticker-instrument-id reborn)))))))

;;;;
;;;; Data retrievals
;;;;

(behavior 'live-data-retrievals
  (with-live-schema ()
    (let* ((source (data-source-by-name "CoinGecko"))
           (retrieval (record-retrieval
                        (data-source-id source)
                        :retrieved-at (test-time 0)
                        :endpoint "https://api.coingecko.com/api/v3/coins/bitcoin"
                        :notes "live test retrieval")))
      (should-be-a 'data-retrieval retrieval)
      (should-be-true (data-retrieval-id retrieval))
      (should= (test-time 0) (data-retrieval-retrieved-at retrieval))
      (should-string= "https://api.coingecko.com/api/v3/coins/bitcoin"
                      (data-retrieval-endpoint retrieval))
      (let ((same (retrieval-by-id (data-retrieval-id retrieval))))
        (should-string= (data-retrieval-id retrieval)
                        (data-retrieval-id same)))
      (should-string= "CoinGecko" (data-retrieval-source-name retrieval))
      (should-string= "CoinGecko"
                      (data-source-name (data-retrieval-source retrieval)))
      (should= 1 (length (retrievals-for-source (data-source-id source))))
      (should-be-null (retrieval-by-id "00000000-0000-0000-0000-000000000000")))))

;;;;
;;;; Ingesting and reading candlesticks
;;;;

(behavior 'live-ingest
  (with-live-schema ()
    (multiple-value-bind (inserted retrieval)
        (ingest-candlesticks "BTC" "USD" "d"
                             (list (list (test-time 0)
                                         25000.0 26000.0 24000.0 25500.0 100.0)
                                   (list (test-time 1)
                                         25500.0 27000.0 25000.0 26500.0 150.0))
                             :source-name "CoinGecko"
                             :endpoint "https://api.coingecko.com/api/v3/coins/bitcoin/market_chart"
                             :notes "live test ingest"
                             :numerator-name "Bitcoin"
                             :numerator-type "cryptocurrency"
                             :denominator-name "US Dollar"
                             :denominator-type "currency")
      (should= 2 inserted)
      (should-be-a 'data-retrieval retrieval)
      (should-string= "CoinGecko" (data-retrieval-source-name retrieval))
      (should= 2 (count-candlesticks "BTC" "USD" "d")))))

(behavior 'live-get
  (with-live-schema ()
    (let ((bars (get-candlesticks "BTC" "USD" "d")))
      (should= 2 (length bars))
      (should-be-a 'candlestick (first bars))
      ;; Time-ascending.
      (should= (test-time 0) (candlestick-time (first bars)))
      (should= (test-time 1) (candlestick-time (second bars)))
      ;; The OHLC values round-trip.
      (should= 25000.0 (candlestick-open (first bars)))
      (should= 26000.0 (candlestick-high (first bars)))
      (should= 24000.0 (candlestick-low (first bars)))
      (should= 25500.0 (candlestick-close (first bars)))
      (should= 100.0 (candlestick-volume (first bars)))
      (should-be-null (candlestick-adjusted-close (first bars)))
      ;; Every bar knows its numerator, denominator, and duration.
      (should-string= (instrument-id (instrument-for-ticker "BTC"
                                                           :source "CoinGecko"))
                      (candlestick-numerator-id (first bars)))
      (should-string= (instrument-id (instrument-for-ticker "USD"
                                                           :source "CoinGecko"))
                      (candlestick-denominator-id (first bars)))
      (should-string= (duration-id (duration-by-short-name "d"))
                      (candlestick-duration-id (first bars)))
      (should-string= (candlestick-retrieval-id (first bars))
                      (candlestick-retrieval-id (second bars)))
      ;; Projections.
      (should-equal (list (test-time 0) (test-time 1))
                    (candlestick-times bars))
      (should-equal '(25500.0 26500.0) (candlestick-close-prices bars))
      (should-equal (list (list (candlestick-time (first bars))
                                (candlestick-close (first bars)))
                          (list (candlestick-time (second bars))
                                (candlestick-close (second bars))))
                    (close-series bars)))))

(behavior 'live-adjusted-close
  (with-live-schema ()
    (let ((usd (instrument-for-ticker "USD" :source "CoinGecko"))
          (aapl (make-instrument :name "Apple" :instrument-type "stock")))
      (make-ticker aapl "AAPL" :source "Yahoo Finance" :exchange "nasdaq")
      (make-ticker usd "USD" :source "Yahoo Finance" :exchange "nasdaq"))
    (ingest-candlesticks "AAPL" "USD" "d"
                         (list (list (test-time 0)
                                     100.0 110.0 95.0 105.0 1000.0 52.5))
                         :source-name "Yahoo Finance"
                         :exchange "nasdaq"
                         :numerator-name "Apple"
                         :numerator-type "stock")
    (let ((bars (get-candlesticks "AAPL" "USD" "d"
                                  :source "Yahoo Finance"
                                  :exchange "nasdaq")))
      (should= 1 (length bars))
      (should= 105.0 (candlestick-close (first bars)))
      (should= 52.5 (candlestick-adjusted-close (first bars)))
      (should-equal (list (list (test-time 0) 52.5))
                    (adjusted-close-series bars)))))

(behavior 'live-get-filters
  (with-live-schema ()
    (let ((halfway (+ (test-time 0) 43200)))
      ;; FROM and TO bound the bar time, inclusive.
      (should= 1 (length (get-candlesticks "BTC" "USD" "d" :from (test-time 1))))
      (should= 1 (length (get-candlesticks "BTC" "USD" "d" :to (test-time 0))))
      (should= 1 (length (get-candlesticks "BTC" "USD" "d"
                                           :from (test-time 0)
                                           :to halfway)))
      (should= 2 (length (get-candlesticks "BTC" "USD" "d"
                                           :from (test-time 0)
                                           :to (test-time 1))))
      (should= 0 (length (get-candlesticks "BTC" "USD" "d"
                                           :from (+ (test-time 1) 1))))
      ;; SOURCE restricts to bars retrieved from that data source.
      (should= 2 (length (get-candlesticks "BTC" "USD" "d" :source "CoinGecko")))
      (should= 0 (length (get-candlesticks "BTC" "USD" "d" :source "No Such Source")))
      ;; Other pairs and durations are empty.
      (should= 0 (length (get-candlesticks "ETH" "USD" "d")))
      (should= 0 (length (get-candlesticks "BTC" "USD" "w"))))))

;;;;
;;;; Provenance and duplicates
;;;;

(behavior 'live-duplicates
  ;; The same bars attached to the same retrieval are a no-op; a new
  ;; retrieval of the same numbers is a second instance of the data.
  (with-live-schema ()
    (let ((ret-id (candlestick-retrieval-id
                   (first (get-candlesticks "BTC" "USD" "d"
                                            :source "CoinGecko")))))
      (multiple-value-bind (inserted ignored)
          (insert-candlesticks
           (list (list (test-time 0)
                       25000.0 26000.0 24000.0 25500.0 100.0))
           :numerator "BTC" :denominator "USD" :duration "d"
           :source-name "CoinGecko"
           :retrieval-id ret-id)
        (declare (ignore ignored))
        (should= 0 inserted)))
    (should= 2 (count-candlesticks "BTC" "USD" "d"))))

(behavior 'live-second-source
  ;; The same bar from a different source is a distinct instance of the data.
  ;; Yahoo's BTC/USD tickers are attached to the same instruments CoinGecko
  ;; already uses; a new source does not create a new Bitcoin.
  (with-live-schema ()
    (let ((btc (instrument-for-ticker "BTC" :source "CoinGecko"))
          (usd (instrument-for-ticker "USD" :source "CoinGecko")))
      (make-ticker btc "BTC" :source "Yahoo Finance")
      (make-ticker usd "USD" :source "Yahoo Finance"))
    (multiple-value-bind (inserted retrieval)
        (ingest-candlesticks "BTC" "USD" "d"
                             (list (list (test-time 0)
                                         25000.0 26000.0 24000.0 25500.0 100.0))
                             :source-name "Yahoo Finance"
                             :endpoint "https://query1.finance.yahoo.com/v8/finance/chart/BTC-USD")
      (should= 1 inserted)
      (should-string= "Yahoo Finance" (data-retrieval-source-name retrieval)))
    (let ((bars (get-candlesticks "BTC" "USD" "d")))
      (should= 3 (length bars))
      ;; The two instances of the test-time 0 bar carry different retrievals.
      (let ((t0 (remove-if-not (lambda (c)
                                 (= (candlestick-time c) (test-time 0)))
                               bars)))
        (should= 2 (length t0))
        (should-not-string= (candlestick-retrieval-id (first t0))
                            (candlestick-retrieval-id (second t0))))
      (should= 2 (length (get-candlesticks "BTC" "USD" "d" :source "CoinGecko")))
      (should= 1 (length (get-candlesticks "BTC" "USD" "d" :source "Yahoo Finance"))))))

;;;;
;;;; Several simultaneous connections
;;;;

(behavior 'live-multiple-connections
  (with-live-schema ()
    (let ((outer (count-candlesticks "BTC" "USD" "d")))
      ;; An inner connection to the same database does not disturb the outer.
      (with-live-schema ()
        (should= outer (count-candlesticks "BTC" "USD" "d")))
      (should= outer (count-candlesticks "BTC" "USD" "d")))))

(behavior 'live-ticker-collision
  ;; The same abbreviation at two sources can name two instruments.
  (with-live-schema ()
    (let ((stock (make-instrument :name "BTC stand-in"
                                  :instrument-type "stock")))
      (make-ticker stock "BTC" :source "Yahoo Finance" :exchange "nyse")
      (should-be-null (instrument-for-ticker "BTC"))
      (should-string= "Bitcoin"
                      (instrument-name
                       (instrument-for-ticker "BTC" :source "CoinGecko")))
      (should-string= "BTC stand-in"
                      (instrument-name
                       (instrument-for-ticker "BTC"
                                              :source "Yahoo Finance"
                                              :exchange "nyse")))
      ;; Ambiguous ticker strings do not resolve on read.
      (should= 0 (length (get-candlesticks "BTC" "USD" "d")))
      (should= 2 (length (get-candlesticks "BTC" "USD" "d"
                                           :source "CoinGecko"))))))

;;;;
;;;; Clean up
;;;;

;; Drop the test schema so the development database is left as we found it.
(with-connection (live-spec)
  (postmodern:execute (format nil "drop schema if exists ~A cascade"
                              *live-schema*)))
