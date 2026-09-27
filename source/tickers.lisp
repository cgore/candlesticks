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

(defpackage :candlesticks/tickers
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config
        :candlesticks/instruments
        :candlesticks/data-sources
        :candlesticks/exchanges)
  (:export :ticker
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
           :ensure-instrument-for-ticker))
(in-package :candlesticks/tickers)

(defclass ticker ()
  ((id
    :initarg :id
    :accessor ticker-id
    :type string
    :initform ""
    :documentation "The UUID of this ticker.")
   (instrument-id
    :initarg :instrument-id
    :accessor ticker-instrument-id
    :type string
    :initform ""
    :documentation "The UUID of the instrument this abbreviation refers to.")
   (source-id
    :initarg :source-id
    :accessor ticker-source-id
    :type string
    :initform ""
    :documentation "The UUID of the data source that uses this abbreviation.")
   (exchange-id
    :initarg :exchange-id
    :accessor ticker-exchange-id
    :initform nil
    :documentation "The UUID of the exchange this listing is on, or NIL
   when the source does not pin a venue (an aggregator).")
   (symbol
    :initarg :symbol
    :accessor ticker-symbol
    :type string
    :initform ""
    :documentation "The abbreviation as that source wrote it, e.g. \"btc\".")
   (canonical
    :initarg :canonical
    :accessor ticker-canonical
    :type string
    :initform ""
    :documentation "The abbreviation, trimmed and upper-cased, e.g. \"BTC\".")
   (valid-from
    :initarg :valid-from
    :accessor ticker-valid-from
    :initform nil
    :documentation "Universal time this abbreviation started meaning this
   instrument at this source, or NIL for unbounded (the beginning).")
   (valid-to
    :initarg :valid-to
    :accessor ticker-valid-to
    :initform nil
    :documentation "Universal time this abbreviation stopped meaning this
   instrument (exclusive), or NIL if it is still valid."))
  (:documentation
   "One abbreviation a data source uses for an instrument, optionally on a
   particular exchange, during a validity window.  CoinGecko's \"BTC\"
   (no exchange) and Yahoo's \"BTC\" on NYSE can be different instruments.
   The same (source, canonical, exchange) may be reused across
   non-overlapping windows: old GM vs new GM."))

(defun tickerp (object)
  "True if OBJECT is a TICKER."
  (typep object 'ticker))

(defmethod print-object ((ticker ticker) stream)
  (print-unreadable-object (ticker stream :type t)
    (format stream "~A" (ticker-canonical ticker))))

(defun canonicalize-ticker (symbol)
  "Trim and upper-case a ticker SYMBOL so that \"btc\", \" BTC \" and
   \"Btc\" match.  This is matching within one source, not a global unique
   name."
  (string-upcase
   (string-trim '(#\Space #\Tab #\Return #\Newline) (string symbol))))

(behavior 'canonicalize-ticker
  (should-string= "BTC" (canonicalize-ticker "btc"))
  (should-string= "AAPL" (canonicalize-ticker "aapl"))
  (should-string= "BTC" (canonicalize-ticker "  btc  "))
  (should-string= "BRK.B" (canonicalize-ticker "brk.b")))

(defun resolve-source-id (source)
  "The UUID of SOURCE, which may be a DATA-SOURCE or a source name.
   A missing name is created."
  (cond ((null source)
         (error "A data source is required to resolve a ticker."))
        ((stringp source)
         (data-source-id-for source))
        (t (data-source-id source))))

(defun lookup-source-id (source)
  "The UUID of SOURCE without creating a row, or NIL."
  (cond ((null source) nil)
        ((stringp source)
         (let ((found (data-source-by-name source)))
           (and found (data-source-id found))))
        (t (data-source-id source))))

(defparameter *ticker-select*
  "select id, instrument_id, data_source_id, exchange_id, symbol, canonical,
          valid_from, valid_to
   from tickers"
  "The SELECT used to load a ticker.")

(defun unbounded-timestamptz-p (value)
  "True when VALUE is a PostgreSQL unbounded timestamptz bound."
  (or (null value)
      (eq value :null)
      (and (stringp value)
           (search "infinity" value :test #'char-equal))
      ;; -infinity as a universal time is before 1900-01-01 (universal time 0).
      (and (integerp value) (minusp value))))

(defun ticker-bound-from-db (value)
  "A timestamptz bound as a universal time, or NIL if unbounded."
  (if (unbounded-timestamptz-p value)
      nil
      (coerce-time value)))

(defun ticker-from->db (universal-time)
  "UNIVERSAL-TIME or NIL (unbounded start) as a timestamptz parameter."
  (if universal-time
      (universal-time->timestamptz universal-time)
      "-infinity"))

(defun ticker-to->db (universal-time)
  "UNIVERSAL-TIME or NIL (still valid) as a timestamptz parameter."
  (if universal-time
      (universal-time->timestamptz universal-time)
      :null))

(defun ticker-valid-at-sql ()
  "SQL fragment: this ticker's window covers $4::timestamptz.  The range is
   [valid_from, valid_to)."
  " and valid_from <= $4::timestamptz
    and (valid_to is null or valid_to > $4::timestamptz)")

(defun row->ticker (row)
  "Build a TICKER from a row of
   (id instrument-id source-id exchange-id symbol canonical
    valid-from valid-to)."
  (make-instance 'ticker
                 :id (first row)
                 :instrument-id (second row)
                 :source-id (third row)
                 :exchange-id (from-db (fourth row))
                 :symbol (fifth row)
                 :canonical (sixth row)
                 :valid-from (ticker-bound-from-db (seventh row))
                 :valid-to (ticker-bound-from-db (eighth row))))

(defun ticker-by-id (id)
  "The TICKER with the given UUID, or NIL."
  (let ((row (first-row
              (concatenate 'string *ticker-select* " where id = $1")
              id)))
    (and row (row->ticker row))))

(defun make-ticker (instrument symbol &key source (exchange nil)
                    valid-from valid-to (at nil at-supplied-p))
  "Attach SYMBOL as a ticker for INSTRUMENT at SOURCE, optionally on
   EXCHANGE, during [VALID-FROM, VALID-TO).  NIL VALID-FROM is unbounded
   (the beginning); NIL VALID-TO means still valid.  AT (default: now)
   selects which existing window to return: if that (source, canonical,
   exchange) is already valid at AT, the existing ticker is returned and
   is not reassigned to a different instrument.  To reuse an abbreviation
   for a new instrument, CLOSE-TICKER the old row first."
  (let ((canonical (canonicalize-ticker symbol))
        (source-id (resolve-source-id source))
        (exchange-id (exchange-id-for exchange))
        (instrument-id (if (instrumentp instrument)
                           (instrument-id instrument)
                           instrument))
        (at (if at-supplied-p
                (or at (get-universal-time))
                (or valid-from (get-universal-time)))))
    (let ((existing (ticker-by-source-id-and-canonical
                     source-id canonical
                     :exchange-id exchange-id
                     :at at)))
      (if existing
          existing
          (let ((id (postmodern:query
                     "insert into tickers
                          (instrument_id, data_source_id, exchange_id,
                           symbol, canonical, valid_from, valid_to)
                      values ($1, $2, $3, $4, $5, $6::timestamptz,
                              $7::timestamptz)
                      returning id"
                     instrument-id source-id (to-db exchange-id)
                     symbol canonical
                     (ticker-from->db valid-from)
                     (ticker-to->db valid-to)
                     :single)))
            (ticker-by-id id))))))

(defun close-ticker (ticker &optional (at (get-universal-time)))
  "Set TICKER's valid-to to AT (a universal time, exclusive).  After that
   the abbreviation can be attached to a different instrument from AT
   onward."
  (let ((id (if (tickerp ticker) (ticker-id ticker) ticker)))
    (postmodern:query
     "update tickers set valid_to = $2::timestamptz where id = $1"
     id (universal-time->timestamptz at))
    (ticker-by-id id)))

(defun ticker-by-source-id-and-canonical (source-id canonical
                                         &key (exchange-id nil)
                                         (at (get-universal-time)))
  "The TICKER for CANONICAL at SOURCE-ID (and optional EXCHANGE-ID) valid
   at AT, or NIL.  AT is a universal time; the window is [valid_from,
   valid_to)."
  (let ((row (first-row
              (concatenate 'string *ticker-select*
                           " where data_source_id = $1
                               and canonical = $2
                               and (($3::uuid is null and exchange_id is null)
                                    or exchange_id = $3::uuid)"
                           (ticker-valid-at-sql)
                           " limit 1")
              source-id canonical (to-db exchange-id)
              (universal-time->timestamptz at))))
    (and row (row->ticker row))))

(defun ticker-by-source-and-symbol (source symbol &key (exchange nil)
                                   (at (get-universal-time)))
  "The TICKER for SYMBOL at SOURCE (and optional EXCHANGE) valid at AT, or
   NIL.  Does not create a row.  AT is a universal time (default: now)."
  (let ((source-id (lookup-source-id source)))
    (and source-id
         (ticker-by-source-id-and-canonical
          source-id (canonicalize-ticker symbol)
          :exchange-id (lookup-exchange-id exchange)
          :at at))))

(defun tickers-for-instrument (instrument)
  "Every ticker attached to INSTRUMENT, including expired windows."
  (let ((id (if (instrumentp instrument)
                (instrument-id instrument)
                instrument)))
    (mapcar #'row->ticker
            (postmodern:query
             (concatenate 'string *ticker-select*
                          " where instrument_id = $1
                            order by canonical, valid_from")
             id :rows))))

(defun tickers-for-symbol (symbol)
  "Every ticker whose canonical abbreviation is SYMBOL, across all sources
   and validity windows."
  (mapcar #'row->ticker
          (postmodern:query
           (concatenate 'string *ticker-select*
                        " where canonical = $1
                          order by data_source_id, valid_from")
           (canonicalize-ticker symbol) :rows)))

(defun instrument-for-ticker (symbol &key source (exchange nil)
                             (at (get-universal-time)))
  "The INSTRUMENT that SOURCE calls SYMBOL (on EXCHANGE, when given) at
   AT, or NIL.  Does not create a row.  When SOURCE is NIL, the instrument
   is returned only if every matching ticker valid at AT points at the
   same instrument; otherwise NIL, so an ambiguous abbreviation is not
   guessed."
  (if source
      (let ((ticker (ticker-by-source-and-symbol source symbol
                                                 :exchange exchange
                                                 :at at)))
        (and ticker (instrument-by-id (ticker-instrument-id ticker))))
      (let* ((tickers (remove-if-not
                       (lambda (tk)
                         (and (or (null (ticker-valid-from tk))
                                  (<= (ticker-valid-from tk) at))
                              (or (null (ticker-valid-to tk))
                                  (> (ticker-valid-to tk) at))))
                       (tickers-for-symbol symbol)))
             (ids (remove-duplicates (mapcar #'ticker-instrument-id tickers)
                                     :test #'string-equal)))
        (and (= 1 (length ids))
             (instrument-by-id (first ids))))))

(defun ensure-instrument-for-ticker (symbol &key source (exchange nil)
                                    name instrument-type
                                    (at (get-universal-time))
                                    valid-from valid-to)
  "Find the instrument that SOURCE calls SYMBOL (on EXCHANGE, when given)
   at AT, or create both the instrument and the ticker.  NAME and
   INSTRUMENT-TYPE fill in missing fields on an existing instrument.  To
   alias another source onto an instrument you already have, call
   MAKE-TICKER with that instrument."
  (let* ((source-id (resolve-source-id source))
         (exchange-id (exchange-id-for exchange))
         (existing (ticker-by-source-id-and-canonical
                    source-id (canonicalize-ticker symbol)
                    :exchange-id exchange-id
                    :at at)))
    (if existing
        (fill-instrument (instrument-by-id (ticker-instrument-id existing))
                         :name name
                         :instrument-type instrument-type)
        (let ((instrument (make-instrument :name name
                                           :instrument-type instrument-type)))
          (make-ticker instrument symbol
                       :source source :exchange exchange
                       :valid-from valid-from :valid-to valid-to
                       :at at)
          instrument))))

(behavior 'ticker
  (let ((tk (make-instance 'ticker
                           :id "tk1" :instrument-id "i1" :source-id "s1"
                           :symbol "btc" :canonical "BTC" :exchange-id nil
                           :valid-from nil :valid-to nil)))
    (should-be-a 'ticker tk)
    (should-be-true (tickerp tk))
    (should-string= "tk1" (ticker-id tk))
    (should-string= "i1" (ticker-instrument-id tk))
    (should-string= "BTC" (ticker-canonical tk))
    (should-string= "btc" (ticker-symbol tk))
    (should-be-null (ticker-exchange-id tk))
    (should-be-null (ticker-valid-from tk))
    (should-be-null (ticker-valid-to tk))))

(behavior 'unbounded-timestamptz-p
  (should-be-true (unbounded-timestamptz-p nil))
  (should-be-true (unbounded-timestamptz-p :null))
  (should-be-true (unbounded-timestamptz-p "-infinity"))
  (should-be-true (unbounded-timestamptz-p "infinity"))
  (should-be-null (unbounded-timestamptz-p
                   (encode-universal-time 0 0 0 1 1 2000 0)))
  (should-be-true (unbounded-timestamptz-p -1)))
