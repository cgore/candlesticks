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

(defpackage :candlesticks/exchanges
  (:use :common-lisp
        :sigma/behave
        :candlesticks/config)
  (:export :exchange
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
           :ensure-standard-exchanges))
(in-package :candlesticks/exchanges)

(defclass exchange ()
  ((id
    :initarg :id
    :accessor exchange-id
    :type string
    :initform ""
    :documentation "The UUID of this exchange.")
   (name
    :initarg :name
    :accessor exchange-name
    :type string
    :initform ""
    :documentation "The canonical slug, e.g. \"nasdaq\" or
   \"uniswap-v3-ethereum\".")
   (display-name
    :initarg :display-name
    :accessor exchange-display-name
    :initform nil
    :documentation "A human-readable name, e.g. \"NASDAQ\".")
   (mic
    :initarg :mic
    :accessor exchange-mic
    :initform nil
    :documentation "Optional ISO 10383 market identifier code, e.g. \"XNAS\".")
   (chain
    :initarg :chain
    :accessor exchange-chain
    :initform nil
    :documentation "Optional chain for a DEX, e.g. \"ethereum\" or
   \"solana\".  NIL for a centralized or traditional exchange.")
   (description
    :initarg :description
    :accessor exchange-description
    :initform nil
    :documentation "An optional human-readable description."))
  (:documentation
   "A venue where instruments trade: a stock exchange, a centralized crypto
   exchange, or a DEX.  This is not a data source (who told us) and not a
   market in the pair-on-a-venue sense (that is numerator/denominator plus
   this exchange)."))

(defun exchange-p (object)
  "True if OBJECT is an EXCHANGE."
  (typep object 'exchange))

(defmethod print-object ((exchange exchange) stream)
  (print-unreadable-object (exchange stream :type t)
    (format stream "~A" (or (exchange-display-name exchange)
                            (exchange-name exchange)))))

(defun row->exchange (row)
  "Build an EXCHANGE from a row of
   (id name display-name mic chain description)."
  (make-instance 'exchange
                 :id (first row)
                 :name (second row)
                 :display-name (from-db (third row))
                 :mic (from-db (fourth row))
                 :chain (from-db (fifth row))
                 :description (from-db (sixth row))))

(defparameter *standard-exchanges*
  '(("nasdaq" "NASDAQ" "XNAS" nil "US stock exchange")
    ("nyse" "New York Stock Exchange" "XNYS" nil "US stock exchange")
    ("binance" "Binance" nil nil "Centralized crypto exchange")
    ("coinbase" "Coinbase" nil nil "Centralized crypto exchange")
    ("kraken" "Kraken" nil nil "Centralized crypto exchange")
    ("uniswap-v3-ethereum" "Uniswap V3 (Ethereum)" nil "ethereum"
     "Automated market maker DEX")
    ("uniswap-v3-arbitrum" "Uniswap V3 (Arbitrum)" nil "arbitrum"
     "Automated market maker DEX")
    ("uniswap-v2-ethereum" "Uniswap V2 (Ethereum)" nil "ethereum"
     "Automated market maker DEX")
    ("sushiswap-ethereum" "SushiSwap (Ethereum)" nil "ethereum"
     "Automated market maker DEX")
    ("curve-ethereum" "Curve (Ethereum)" nil "ethereum"
     "Stableswap DEX")
    ("pancakeswap-bsc" "PancakeSwap (BSC)" nil "bsc"
     "Automated market maker DEX")
    ("orca-solana" "Orca (Solana)" nil "solana"
     "Automated market maker DEX")
    ("raydium-solana" "Raydium (Solana)" nil "solana"
     "Automated market maker DEX"))
  "Conventional exchanges, as (name display-name mic chain description).")

(defun standard-exchanges ()
  "The conventional exchange tuples.  A fresh copy is returned."
  (mapcar #'(lambda (d) (copy-list d)) *standard-exchanges*))

(defun make-exchange (name &key display-name mic chain description)
  "Find or create the EXCHANGE with the given NAME and return it.  NAME is
   canonicalized (trimmed and down-cased).  Optional fields fill in missing
   values on an existing exchange."
  (let ((canonical (canonicalize-slug name)))
    (let ((row (postmodern:query
                "insert into exchanges
                     (name, display_name, mic, chain, description)
                 values ($1, $2, $3, $4, $5)
                 on conflict (name) do update
                   set display_name = coalesce(exchanges.display_name,
                                               excluded.display_name),
                       mic = coalesce(exchanges.mic, excluded.mic),
                       chain = coalesce(exchanges.chain, excluded.chain),
                       description = coalesce(exchanges.description,
                                              excluded.description)
                 returning id, name, display_name, mic, chain, description"
                canonical
                (to-db display-name) (to-db mic) (to-db chain)
                (to-db description) :row)))
      (row->exchange row))))

(defun exchange-by-name (name)
  "The EXCHANGE whose canonical name is NAME, or NIL."
  (let ((row (first-row
              "select id, name, display_name, mic, chain, description
               from exchanges where name = $1 limit 1"
              (canonicalize-slug name))))
    (and row (row->exchange row))))

(defun exchange-id-for (exchange)
  "The database UUID of EXCHANGE, or NIL when EXCHANGE is NIL.  EXCHANGE
   may be an EXCHANGE object or a name string (which is found or created)."
  (cond ((null exchange) nil)
        ((exchange-p exchange)
         (exchange-id exchange))
        ((stringp exchange)
         (exchange-id (make-exchange exchange)))
        (t (error "Cannot resolve the exchange ~S" exchange))))

(defun lookup-exchange-id (exchange)
  "The UUID of EXCHANGE without creating a row, or NIL."
  (cond ((null exchange) nil)
        ((exchange-p exchange)
         (exchange-id exchange))
        ((stringp exchange)
         (let ((found (exchange-by-name exchange)))
           (and found (exchange-id found))))
        (t (exchange-id exchange))))

(defun all-exchanges ()
  "All exchanges, alphabetically by name."
  (mapcar #'row->exchange
          (postmodern:query
           "select id, name, display_name, mic, chain, description
            from exchanges order by name"
           :rows)))

(defun ensure-standard-exchanges ()
  "Insert the standard stock, CEX, and DEX venues if they are not already
   present, and return them."
  (mapcar (lambda (d)
            (make-exchange (first d)
                           :display-name (second d)
                           :mic (third d)
                           :chain (fourth d)
                           :description (fifth d)))
          *standard-exchanges*))

(behavior 'standard-exchanges
  (should= 13 (length *standard-exchanges*))
  (should-equal '("nasdaq" "NASDAQ" "XNAS" nil "US stock exchange")
                (first *standard-exchanges*))
  (should-string= "uniswap-v3-ethereum"
                  (first (sixth *standard-exchanges*)))
  (let ((copy (standard-exchanges)))
    (setf (first (first copy)) "mutated")
    (should-string= "nasdaq" (first (first *standard-exchanges*)))))

(behavior 'exchange
  (let ((ex (make-instance 'exchange
                           :id "e1" :name "binance" :display-name "Binance"
                           :chain nil)))
    (should-be-a 'exchange ex)
    (should-be-true (exchange-p ex))
    (should-string= "e1" (exchange-id ex))
    (should-string= "binance" (exchange-name ex))
    (should-string= "Binance" (exchange-display-name ex))))
