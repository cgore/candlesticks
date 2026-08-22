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

;;; The Candlesticks umbrella package.  It re-exports every symbol from the
;;; sub-packages, so that (use-package :candlesticks) gives you the whole
;;; library at once.

(defpackage :candlesticks
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
  (:export
    ;; Connections and shared helpers (candlesticks/config).
    :*default-connection-spec*
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
    :first-row

    ;; Durations (candlesticks/durations).
    :duration
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
    :durationp

    ;; Instrument types (candlesticks/instrument-types).
    :instrument-type
    :instrument-type-id
    :instrument-type-name
    :instrument-type-description
    :instrument-type-p
    :make-instrument-type
    :instrument-type-by-name
    :instrument-type-id-for
    :all-instrument-types
    :standard-instrument-types
    :ensure-standard-instrument-types

    ;; Instruments (candlesticks/instruments).
    :instrument
    :instrument-id
    :instrument-name
    :make-instrument
    :fill-instrument
    :instrument-by-id
    :all-instruments
    :instrumentp

    ;; Tickers (candlesticks/tickers).
    :ticker
    :ticker-id
    :ticker-instrument-id
    :ticker-source-id
    :ticker-exchange-id
    :ticker-symbol
    :ticker-canonical
    :ticker-valid-from
    :ticker-valid-to
    :tickerp
    :canonicalize-ticker
    :make-ticker
    :close-ticker
    :ticker-by-id
    :ticker-by-source-and-symbol
    :tickers-for-instrument
    :tickers-for-symbol
    :instrument-for-ticker
    :ensure-instrument-for-ticker

    ;; Data source kinds (candlesticks/data-source-kinds).
    :data-source-kind
    :data-source-kind-id
    :data-source-kind-name
    :data-source-kind-description
    :data-source-kind-p
    :make-data-source-kind
    :data-source-kind-by-name
    :data-source-kind-id-for
    :all-data-source-kinds
    :standard-data-source-kinds
    :ensure-standard-data-source-kinds

    ;; Exchanges (candlesticks/exchanges).
    :exchange
    :exchange-id
    :exchange-name
    :exchange-display-name
    :exchange-mic
    :exchange-chain
    :exchange-description
    :exchange-p
    :make-exchange
    :exchange-by-name
    :exchange-id-for
    :lookup-exchange-id
    :all-exchanges
    :standard-exchanges
    :ensure-standard-exchanges

    ;; Data sources (candlesticks/data-sources).
    :data-source
    :data-source-id
    :data-source-name
    :make-data-source
    :data-source-by-name
    :data-source-id-for
    :all-data-sources

    ;; Data retrievals (candlesticks/data-retrievals).
    :data-retrieval
    :data-retrieval-id
    :data-retrieval-source-id
    :data-retrieval-source
    :data-retrieval-source-name
    :data-retrieval-exchange-id
    :data-retrieval-exchange
    :data-retrieval-exchange-name
    :data-retrieval-retrieved-at
    :data-retrieval-endpoint
    :data-retrieval-notes
    :record-retrieval
    :retrieval-by-id
    :retrievals-for-source

    ;; Candlesticks (candlesticks/bars).
    :candlestick
    :candlestick-id
    :candlestick-numerator-id
    :candlestick-denominator-id
    :candlestick-duration-id
    :candlestick-retrieval-id
    :candlestick-time
    :candlestick-open
    :candlestick-high
    :candlestick-low
    :candlestick-close
    :candlestick-volume
    :candlestick-adjusted-close
    :instrument-id-for
    :duration-id-for
    :insert-candlestick
    :insert-candlesticks
    :ingest-candlesticks
    :get-candlesticks
    :count-candlesticks
    :candlestick-times
    :candlestick-open-prices
    :candlestick-high-prices
    :candlestick-low-prices
    :candlestick-close-prices
    :candlestick-volumes
    :candlestick-adjusted-close-prices
    :close-series
    :adjusted-close-series

    ;; Migrations (candlesticks/migrations).
    :migrations-path
    :connection-spec->migratum-spec
    :with-migration-driver
    :run-migrations
    :revert-last-migration
    :pending-migrations
    :applied-migrations
    :latest-migration-id
    :migration-status?

    ;; Version (candlesticks/system).
    :version-string
    :version-list
    :version-major
    :version-minor
    :version-revision))
(in-package :candlesticks)

(behavior 'version-string
  (should-equal '(0 1 0) (version-list))
  (should-string= "0.1.0" (version-string)))

(behavior 'use-all-symbols
  (should-be-true (find-symbol "CANDLESTICK" (find-package :candlesticks)))
  (should-be-true (find-symbol "INGEST-CANDLESTICKS" (find-package :candlesticks)))
  (should-be-true (find-symbol "WITH-CONNECTION" (find-package :candlesticks)))
  (should-be-true (find-symbol "RUN-MIGRATIONS" (find-package :candlesticks)))
  (should-be-true (find-symbol "INSTRUMENTP" (find-package :candlesticks)))
  (should-be-true (find-symbol "DURATIONP" (find-package :candlesticks)))
  (should-be-true (find-symbol "DATA-SOURCE-ID-FOR" (find-package :candlesticks)))
  (should-be-true (find-symbol "DATA-RETRIEVAL-SOURCE-NAME" (find-package :candlesticks)))
  (should-be-true (find-symbol "INSTRUMENT-TYPE" (find-package :candlesticks)))
  (should-be-true (find-symbol "DATA-SOURCE-KIND" (find-package :candlesticks)))
  (should-be-true (find-symbol "ENSURE-STANDARD-INSTRUMENT-TYPES"
                              (find-package :candlesticks)))
  (should-be-true (find-symbol "ENSURE-STANDARD-DATA-SOURCE-KINDS"
                              (find-package :candlesticks)))
  (should-be-true (find-symbol "TICKER" (find-package :candlesticks)))
  (should-be-true (find-symbol "ENSURE-INSTRUMENT-FOR-TICKER"
                              (find-package :candlesticks)))
  (should-be-true (find-symbol "EXCHANGE" (find-package :candlesticks)))
  (should-be-true (find-symbol "ENSURE-STANDARD-EXCHANGES"
                              (find-package :candlesticks))))
