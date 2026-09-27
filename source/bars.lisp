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

(defpackage :candlesticks/bars
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :candlesticks/durations
        :candlesticks/instruments
        :candlesticks/data-sources
        :candlesticks/exchanges
        :candlesticks/tickers
        :candlesticks/data-retrievals)
  (:export :candlestick
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
           :candlestick-is-closed
           :instrument-id-for
           :duration-id-for
           :sanitize-ohlc
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
           :adjusted-close-series))
(in-package :candlesticks/bars)

(defclass candlestick ()
  ((id
    :initarg :id
    :accessor candlestick-id
    :type string
    :initform ""
    :documentation "The UUID of this candlestick.")
   (numerator-id
    :initarg :numerator-id
    :accessor candlestick-numerator-id
    :type string
    :initform ""
    :documentation "The UUID of the numerator instrument, e.g. Bitcoin for
   BTC/USD.")
   (denominator-id
    :initarg :denominator-id
    :accessor candlestick-denominator-id
    :type string
    :initform ""
    :documentation "The UUID of the denominator instrument, e.g. the US Dollar
   for BTC/USD.")
   (duration-id
    :initarg :duration-id
    :accessor candlestick-duration-id
    :type string
    :initform ""
    :documentation "The UUID of the candlestick duration (the time window of
   the bar).")
   (retrieval-id
    :initarg :retrieval-id
    :accessor candlestick-retrieval-id
    :type string
    :initform ""
    :documentation "The UUID of the data retrieval that produced this bar.")
   (time
    :initarg :time
    :accessor candlestick-time
    :documentation "A universal time: the time of the bar (its open).")
   (open
    :initarg :open
    :accessor candlestick-open
    :documentation "The opening price of NUMERATOR denominated in
   DENOMINATOR.")
   (high
    :initarg :high
    :accessor candlestick-high
    :documentation "The high of the bar.")
   (low
    :initarg :low
    :accessor candlestick-low
    :documentation "The low of the bar.")
   (close
    :initarg :close
    :accessor candlestick-close
    :documentation "The closing price of the bar.")
   (volume
    :initarg :volume
    :accessor candlestick-volume
    :initform nil
    :documentation "The volume of the bar, or NIL when the source did not
   provide one.")
   (adjusted-close
    :initarg :adjusted-close
    :accessor candlestick-adjusted-close
    :initform nil
    :documentation "The split-adjusted close as the source reported it, or
   NIL when the source did not provide one.  OPEN/HIGH/LOW/CLOSE stay
   as-traded.")
   (is-closed
    :initarg :is-closed
    :accessor candlestick-is-closed
    :initform t
    :documentation "True when this print was taken after the bar's session
   ended.  An open daily bar, such as today's crypto candle, is false.
   The library stores the bit; the caller decides when the session ends."))
  (:documentation
   "One OHLC candlestick: the as-traded price of NUMERATOR denominated in
   DENOMINATOR over a given DURATION, together with the RETRIEVAL that
   produced it.  ADJUSTED-CLOSE is the source's split-adjusted close when
   it has one (Yahoo does; CoinGecko typically does not).  IS-CLOSED is
   false while the session is still open."))

(defmethod print-object ((candlestick candlestick) stream)
  (print-unreadable-object (candlestick stream :type t)
    (format stream "~A/~A @ ~A: o~A h~A l~A c~A"
            (candlestick-numerator-id candlestick)
            (candlestick-denominator-id candlestick)
            (candlestick-time candlestick)
            (candlestick-open candlestick)
            (candlestick-high candlestick)
            (candlestick-low candlestick)
            (candlestick-close candlestick))))

(defun row->candlestick (row)
  "Build a CANDLESTICK from a row of
   (id numerator-id denominator-id duration-id retrieval-id time
    open high low close volume adjusted-close is-closed)."
  (make-instance 'candlestick
                 :id (elt row 0)
                 :numerator-id (elt row 1)
                 :denominator-id (elt row 2)
                 :duration-id (elt row 3)
                 :retrieval-id (elt row 4)
                 :time (coerce-time (elt row 5))
                 :open (coerce-price (elt row 6))
                 :high (coerce-price (elt row 7))
                 :low (coerce-price (elt row 8))
                 :close (coerce-price (elt row 9))
                 :volume (coerce-price (elt row 10))
                 :adjusted-close (coerce-price (elt row 11))
                 :is-closed (coerce-closed (elt row 12))))

(defun coerce-closed (value)
  "Return T or NIL for a PostgreSQL boolean VALUE."
  (cond ((or (eq value t)
             (and (stringp value)
                  (member value '("t" "true") :test #'string-equal)))
         t)
        ((or (null value)
             (eq value :null)
             (and (stringp value)
                  (member value '("f" "false") :test #'string-equal)))
         nil)
        (t (and value t))))

(defun bar-is-closed? (closed time)
  "T when CLOSED marks the bar at TIME closed.
CLOSED is T, NIL, or a function of the bar's universal time."
  (cond ((eq closed t) t)
        ((null closed) nil)
        ((functionp closed) (and (funcall closed time) t))
        (t (error "CLOSED must be T, NIL, or a function of the bar time, not ~S."
                  closed))))

(behavior 'bar-is-closed
  (should-be-true (bar-is-closed? t 1))
  (should-be-null (bar-is-closed? nil 1))
  (should-be-true (bar-is-closed? (lambda (time) (> time 10)) 11))
  (should-be-null (bar-is-closed? (lambda (time) (> time 10)) 10))
  (should-be-true (candlestick-is-closed (make-instance 'candlestick)))
  (should-be-null (candlestick-is-closed
                   (make-instance 'candlestick :is-closed nil))))

;;;;
;;;; Resolving human input to database identifiers
;;;;

(defun instrument-id-for (instrument &key name instrument-type source exchange
                         (at (get-universal-time)))
  "Return the database UUID of INSTRUMENT.  INSTRUMENT may be an INSTRUMENT
   object, or a ticker string.  A ticker string is resolved (and created if
   needed) at SOURCE, optionally on EXCHANGE, as of AT (a universal time,
   default now).  NAME and INSTRUMENT-TYPE fill in missing fields on the
   instrument."
  (if (instrumentp instrument)
    (instrument-id instrument)
    (instrument-id (ensure-instrument-for-ticker
                    instrument
                    :source source
                    :exchange exchange
                    :name name
                    :instrument-type instrument-type
                    :at at))))

(behavior 'instrument-id-for
  (let ((inst (make-instance 'instrument :id "uuid-1")))
    (should-string= "uuid-1" (instrument-id-for inst))))

(defun duration-id-for (duration)
  "Return the database UUID of DURATION.  DURATION may be a DURATION object,
   a short name (\"d\", \"w\", ...), a long name (\"day\", \"week\", ...), or
   an existing duration UUID."
  (cond ((durationp duration)
         (duration-id duration))
        ((stringp duration)
         (let ((by-short (duration-by-short-name duration))
               (by-long (duration-by-long-name duration)))
           (or (and by-short (duration-id by-short))
               (and by-long (duration-id by-long))
               ;; Fall back to treating the string as an existing UUID.
               duration)))
        (t (error "Cannot resolve the duration ~S" duration))))

(behavior 'duration-id-for
  (let ((d (make-instance 'duration :id "uuid-2")))
    (should-string= "uuid-2" (duration-id-for d))))

;;;;
;;;; Writing
;;;;

(defun sanitize-ohlc (open high low close)
  "Return OPEN HIGH LOW CLOSE with HIGH/LOW expanded to envelope OPEN and
   CLOSE.  Yahoo (and others) occasionally emit a close outside the quoted
   range, which violates candlesticks_check3 (low <= open and low <= close)
   and the matching high checks."
  (let ((o (or open 0))
        (h (or high 0))
        (l (or low 0))
        (c (or close 0)))
    (values o (max h o c) (min l o c) c)))

(behavior 'sanitize-ohlc
  (multiple-value-bind (o h l c)
      (sanitize-ohlc 261.0 261.0 261.0 260.29998779296875)
    (should= 261.0 o)
    (should= 261.0 h)
    (should= 260.29998779296875 l)
    (should= 260.29998779296875 c))
  (multiple-value-bind (o h l c)
      (sanitize-ohlc 10 9 8 11)
    (should= 10 o)
    (should= 11 h)
    (should= 8 l)
    (should= 11 c))
  (multiple-value-bind (o h l c)
      (sanitize-ohlc 5 5 5 5)
    (should= 5 o)
    (should= 5 h)
    (should= 5 l)
    (should= 5 c)))

(defun insert-candlestick (numerator-id denominator-id duration-id
                            retrieval-id time open high low close
                            &key volume adjusted-close (closed t))
  "Insert a single CANDLESTICK given its already-resolved identifiers and
   return the new row.  TIME is a universal time; the price fields are
   numbers; VOLUME and ADJUSTED-CLOSE may be NIL.  CLOSED is T, NIL, or a
   function of TIME; it sets IS-CLOSED."
  (multiple-value-bind (open high low close)
      (sanitize-ohlc open high low close)
    (let ((row (postmodern:query
                "insert into candlesticks
                     (numerator_id, denominator_id, duration_id, data_retrieval_id,
                      time, open, high, low, close, volume, adjusted_close,
                      is_closed)
                 values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
                 on conflict (numerator_id, denominator_id, duration_id, time,
                             data_retrieval_id) do nothing
                 returning id, numerator_id, denominator_id, duration_id,
                           data_retrieval_id, time, open, high, low, close,
                           volume, adjusted_close, is_closed"
                numerator-id denominator-id duration-id retrieval-id
                (universal-time->timestamptz time)
                open high low close
                (to-db volume) (to-db adjusted-close)
                (bar-is-closed? closed time)
                :row)))
      (and row (row->candlestick row)))))

(defun insert-candlestick-rows (rows numerator-id denominator-id duration-id
                                retrieval-id &key (closed t))
  "Insert many candlesticks, one statement per row, and return the number of
   rows inserted.  ROWS is a list of (time open high low close volume) or
   (time open high low close volume adjusted-close) tuples.  CLOSED is T,
   NIL, or a function of each bar's time.  Run this inside a transaction
   (as INGEST-CANDLESTICKS does) so the whole batch commits or rolls back
   together."
  (let ((count 0))
    (dolist (row rows)
      (destructuring-bind (time open high low close volume
                           &optional adjusted-close)
          row
        (multiple-value-bind (open high low close)
            (sanitize-ohlc open high low close)
          (incf count
                (postmodern:execute
                 "insert into candlesticks
              (numerator_id, denominator_id, duration_id, data_retrieval_id,
               time, open, high, low, close, volume, adjusted_close, is_closed)
         values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
         on conflict (numerator_id, denominator_id, duration_id, time,
                     data_retrieval_id) do nothing"
                 numerator-id denominator-id duration-id retrieval-id
                 (universal-time->timestamptz time)
                 open high low close
                 (to-db volume) (to-db adjusted-close)
                 (bar-is-closed? closed time))))))
    count))

(defun insert-candlesticks (rows &key numerator denominator duration
                            (retrieval-id nil) source-name source-kind
                            exchange
                            endpoint notes (retrieved-at (get-universal-time))
                            (at (get-universal-time))
                            (closed t))
  "Insert a batch of candlestick ROWS, each a (time open high low close
   volume) or (time open high low close volume adjusted-close) tuple, and
   return the DATA-RETRIEVAL that records the batch's
   provenance.

   NUMERATOR and DENOMINATOR are instrument symbols or INSTRUMENT objects,
   DURATION a duration (see DURATION-ID-FOR), and SOURCE-NAME the name of the
   data source.  CLOSED is T, NIL, or a function of each bar's universal
   time; it sets IS-CLOSED and defaults to T.  AT is the universal time
   used to resolve ticker strings (default: now).  When RETRIEVAL-ID is
   supplied, the bars are attached to that existing retrieval instead of
   creating a new one."
  (postmodern:with-transaction ()
    (let* ((num-id (instrument-id-for numerator
                                      :source source-name
                                      :exchange exchange
                                      :at at))
           (den-id (instrument-id-for denominator
                                      :source source-name
                                      :exchange exchange
                                      :at at))
           (dur-id (duration-id-for duration))
           (ret-id (or retrieval-id
                       (let ((src-id (data-source-id-for source-name source-kind)))
                         (data-retrieval-id
                          (record-retrieval src-id
                                            :retrieved-at retrieved-at
                                            :exchange exchange
                                            :endpoint endpoint
                                            :notes notes)))))
           (inserted (insert-candlestick-rows rows num-id den-id dur-id ret-id
                                              :closed closed)))
      (if retrieval-id
        (values inserted nil)
        (values inserted (retrieval-by-id ret-id))))))

(behavior 'insert-candlestick-rows
  (should= 0 (insert-candlestick-rows '() "n" "d" "dur" "ret")))

(defun ingest-candlesticks (numerator denominator duration bars
                             &key source-name (source-kind nil)
                             exchange
                             endpoint notes
                             (numerator-name nil) (numerator-type nil)
                             (denominator-name nil) (denominator-type nil)
                             (retrieved-at (get-universal-time))
                             (at (get-universal-time))
                             (closed t))
  "Store a batch of OHLC candlesticks in a single transaction and return the
   DATA-RETRIEVAL that records where they came from.  This is the high-level
   entry point for feeding data in from a source such as CoinGecko or Yahoo
   Finance.

   NUMERATOR and DENOMINATOR are instrument symbols (or INSTRUMENT objects).
   DURATION is a duration (short name, long name, DURATION, or id).  BARS is a
   list of (time open high low close volume) or
   (time open high low close volume adjusted-close) tuples, where TIME is a
   universal time, the price fields are numbers, and VOLUME and
   ADJUSTED-CLOSE may be NIL.  CLOSED is T, NIL, or a function of each bar's
   universal time.  It sets IS-CLOSED and defaults to T, so a history fetch
   is stored as finished unless the caller marks an open session.

   NUMERATOR-NAME / NUMERATOR-TYPE and DENOMINATOR-NAME / DENOMINATOR-TYPE
   are optional names and instrument types for the two instruments.
   SOURCE-NAME, SOURCE-KIND, EXCHANGE, ENDPOINT, NOTES, and RETRIEVED-AT
   are recorded as provenance.  EXCHANGE is the venue the series traded on
   (NASDAQ, Binance, Uniswap V3, ...), not the data source.  AT is the
   universal time used to resolve ticker strings (default: now).  Each call
   records a new retrieval, so ingesting the same numbers again stores a
   second instance of the data.  Inserting the same bars twice against one
   existing retrieval is a no-op."
  (postmodern:with-transaction ()
    (let* ((src-id (data-source-id-for source-name source-kind))
           (num-id (instrument-id-for numerator
                                      :name numerator-name
                                      :instrument-type numerator-type
                                      :source source-name
                                      :exchange exchange
                                      :at at))
           (den-id (instrument-id-for denominator
                                      :name denominator-name
                                      :instrument-type denominator-type
                                      :source source-name
                                      :exchange exchange
                                      :at at))
           (dur-id (duration-id-for duration))
           (ret-id (data-retrieval-id
                    (record-retrieval src-id
                                      :retrieved-at retrieved-at
                                      :exchange exchange
                                      :endpoint endpoint
                                      :notes notes)))
           (inserted (insert-candlestick-rows bars num-id den-id dur-id ret-id
                                              :closed closed)))
      (values inserted (retrieval-by-id ret-id)))))

;;;;
;;;; Reading
;;;;

(defun lookup-instrument-id (instrument &key source exchange
                             (at (get-universal-time)))
  "The UUID of INSTRUMENT without creating a row.  INSTRUMENT may be an
   INSTRUMENT object or a ticker string.  A ticker string is resolved at
   SOURCE (and optional EXCHANGE) when given, as of AT; without SOURCE it
   is returned only if every matching ticker valid at AT points at the
   same instrument.  Returns NIL when it is not found or is ambiguous."
  (if (instrumentp instrument)
      (instrument-id instrument)
      (let ((found (instrument-for-ticker instrument
                                          :source source
                                          :exchange exchange
                                          :at at)))
        (and found (instrument-id found)))))

(defun lookup-duration-id (duration)
  "The UUID of DURATION without creating a row.  DURATION may be a DURATION
   object, a short name, or a long name.  Returns NIL when it is not found."
  (if (durationp duration)
      (duration-id duration)
      (let ((found (or (duration-by-short-name duration)
                       (duration-by-long-name duration))))
        (and found (duration-id found)))))

(defun lookup-data-source-id (source)
  "The UUID of SOURCE without creating a row.  SOURCE may be a DATA-SOURCE
   object or a name string.  Returns NIL when it is not found."
  (if (stringp source)
      (let ((found (data-source-by-name source)))
        (and found (data-source-id found)))
      (data-source-id source)))

(defun get-candlesticks (numerator denominator duration
                         &key from to source exchange
                         (at (get-universal-time)))
  "Return the candlesticks for NUMERATOR/DENOMINATOR at the given DURATION,
   in time-ascending order.

   NUMERATOR and DENOMINATOR are instruments or ticker strings; DURATION a
   duration (short name, long name, DURATION, or id); FROM and TO, when
   given, are universal times bounding the bar time (inclusive).  SOURCE,
   when given, both resolves ticker strings at that data source and
   restricts the result to bars retrieved from it.  EXCHANGE, when given,
   both resolves tickers on that venue and restricts the result to
   retrievals pinned to it.  AT is the universal time used to resolve
   ticker strings (default: now).

   This function never inserts rows.  If the pair, duration, source, or
   exchange does not already exist, or a ticker string is ambiguous, the
   result is the empty list."
  (let ((num-id (lookup-instrument-id numerator
                                      :source source
                                      :exchange exchange
                                      :at at))
        (den-id (lookup-instrument-id denominator
                                      :source source
                                      :exchange exchange
                                      :at at))
        (dur-id (lookup-duration-id duration)))
    (when source
      (unless (lookup-data-source-id source)
        (return-from get-candlesticks nil)))
    (when exchange
      (unless (lookup-exchange-id exchange)
        (return-from get-candlesticks nil)))
    (unless (and num-id den-id dur-id)
      (return-from get-candlesticks nil))
    (let ((src-id (when source (lookup-data-source-id source)))
          (ex-id (when exchange (lookup-exchange-id exchange)))
          (from-str (when from (universal-time->timestamptz from)))
          (to-str (when to (universal-time->timestamptz to))))
      (mapcar #'row->candlestick
              (postmodern:query
               "select c.id, c.numerator_id, c.denominator_id,
                       c.duration_id, c.data_retrieval_id,
                       c.time, c.open, c.high, c.low, c.close, c.volume,
                       c.adjusted_close, c.is_closed
               from candlesticks c
               join data_retrievals r on r.id = c.data_retrieval_id
               where c.numerator_id = $1
                 and c.denominator_id = $2
                 and c.duration_id = $3
                 and ($4::uuid is null or r.data_source_id = $4::uuid)
                 and ($5::uuid is null or r.exchange_id = $5::uuid)
                 and ($6::timestamptz is null or c.time >= $6::timestamptz)
                 and ($7::timestamptz is null or c.time <= $7::timestamptz)
               order by c.time"
               num-id den-id dur-id
               (to-db src-id)
               (to-db ex-id)
               (to-db from-str)
               (to-db to-str)
               :rows)))))

(defun count-candlesticks (numerator denominator duration
                           &key source exchange
                           (at (get-universal-time)))
  "The number of stored candlesticks for NUMERATOR/DENOMINATOR at DURATION,
   summed over all retrievals.  Returns 0 when the pair or duration is not
   already present, or a ticker string is ambiguous; this function never
   inserts rows.  SOURCE and EXCHANGE, when given, resolve ticker strings
   as of AT (default: now)."
  (let ((num-id (lookup-instrument-id numerator
                                      :source source
                                      :exchange exchange
                                      :at at))
        (den-id (lookup-instrument-id denominator
                                      :source source
                                      :exchange exchange
                                      :at at))
        (dur-id (lookup-duration-id duration)))
    (unless (and num-id den-id dur-id)
      (return-from count-candlesticks 0))
    (let ((rows (postmodern:query
                 "select count(*) from candlesticks
                  where numerator_id = $1 and denominator_id = $2
                    and duration_id = $3"
                 num-id den-id dur-id :rows)))
      (first (first rows)))))

;;;;
;;;; Convenience projections
;;;;

(defun candlestick-times (candlesticks)
  "The times of CANDLESTICKS, in the order given."
  (mapcar #'candlestick-time candlesticks))

(defun candlestick-open-prices (candlesticks)
  "The opening prices of CANDLESTICKS, in the order given."
  (mapcar #'candlestick-open candlesticks))

(defun candlestick-high-prices (candlesticks)
  "The high prices of CANDLESTICKS, in the order given."
  (mapcar #'candlestick-high candlesticks))

(defun candlestick-low-prices (candlesticks)
  "The low prices of CANDLESTICKS, in the order given."
  (mapcar #'candlestick-low candlesticks))

(defun candlestick-close-prices (candlesticks)
  "The closing prices of CANDLESTICKS, in the order given.  This is the
   series most often handed to technical indicators."
  (mapcar #'candlestick-close candlesticks))

(defun candlestick-volumes (candlesticks)
  "The volumes of CANDLESTICKS, in the order given (NIL where unknown)."
  (mapcar #'candlestick-volume candlesticks))

(defun candlestick-adjusted-close-prices (candlesticks)
  "The split-adjusted closes of CANDLESTICKS, in the order given (NIL
   where the source did not provide one)."
  (mapcar #'candlestick-adjusted-close candlesticks))

(defun close-series (candlesticks)
  "A list of (time close) pairs for CANDLESTICKS, in the order given -- a
   convenient shape for feeding straight into a time series.  These are
   as-traded closes; see ADJUSTED-CLOSE-SERIES for split-adjusted values."
  (mapcar (lambda (candlestick)
            (list (candlestick-time candlestick)
                  (candlestick-close candlestick)))
          candlesticks))

(defun adjusted-close-series (candlesticks)
  "A list of (time adjusted-close) pairs for CANDLESTICKS, in the order
   given.  ADJUSTED-CLOSE may be NIL when the source did not provide one."
  (mapcar (lambda (candlestick)
            (list (candlestick-time candlestick)
                  (candlestick-adjusted-close candlestick)))
          candlesticks))

(behavior 'candlestick
  (let ((c (make-instance 'candlestick
                          :id "id"
                          :numerator-id "num" :denominator-id "den"
                          :duration-id "dur" :retrieval-id "ret"
                          :time 1700000000
                          :open 100.0 :high 110.0 :low 95.0 :close 105.0
                          :volume 1000.0 :adjusted-close 52.5)))
    (should-be-a 'candlestick c)
    (should-string= "num" (candlestick-numerator-id c))
    (should-string= "den" (candlestick-denominator-id c))
    (should-string= "dur" (candlestick-duration-id c))
    (should-string= "ret" (candlestick-retrieval-id c))
    (should= 1700000000 (candlestick-time c))
    (should= 100.0 (candlestick-open c))
    (should= 110.0 (candlestick-high c))
    (should= 95.0 (candlestick-low c))
    (should= 105.0 (candlestick-close c))
    (should= 1000.0 (candlestick-volume c))
    (should= 52.5 (candlestick-adjusted-close c))
    (should-be-true (candlestick-is-closed c))))

(behavior 'projections
  (let ((first-bar (make-instance 'candlestick
                                  :time 100 :close 10.0 :adjusted-close 5.0))
        (second-bar (make-instance 'candlestick
                                   :time 100 :close 20.0 :adjusted-close 10.0)))
    (should-equal '(10.0 20.0)
                  (candlestick-close-prices (list first-bar second-bar)))
    (should-equal '((100 10.0) (100 20.0))
                  (close-series (list first-bar second-bar)))
    (should-equal '(5.0 10.0)
                  (candlestick-adjusted-close-prices
                   (list first-bar second-bar)))
    (should-equal '((100 5.0) (100 10.0))
                  (adjusted-close-series (list first-bar second-bar)))))
