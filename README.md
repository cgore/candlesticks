# Candlesticks

[![Albrecht Dürer, "The Vision of the Seven Candlesticks"](The_Vision_of_the_Seven_Candlesticks.jpg)](https://commons.wikimedia.org/wiki/File:Albrecht_D%C3%BCrer,_The_Vision_of_the_Seven_Candlesticks,_probably_c._1496-1498,_NGA_848.jpg)

**[View PDF Documentation](documentation/candlesticks.pdf)**

Candlesticks is an interface to a PostgreSQL database of historical,
time-series financial data: OHLC candlesticks for any fungible, traded
instrument -- stocks, bonds, cryptocurrencies, commodities, currencies, and
so on -- together with the provenance of each bar, namely which data source
it came from and the specific retrieval that produced it.

A price is always a pair of instruments.  BTC/USD is Bitcoin as the
numerator and the US Dollar as the denominator.  AAPL/USD is Apple stock in
dollars.  The same tables hold all of them.

The library is meant to become the historical store for
[Livermore](https://github.com/cgore/livermore) (replacing the stock code
currently living in `livermore/source/stocks.lisp`) and for
[limbic.fi](https://limbic.fi) (long-term crypto histories from
[cl-coingecko](https://github.com/cgore/cl-coingecko)).

- Github: [cgore/candlesticks](https://github.com/cgore/candlesticks)

## Install

Clone next to your other local systems (or symlink the `.asd` into
`~/programming/lisp/systems/`):

```lisp
(asdf:load-system :candlesticks)
(in-package :candlesticks)
```

Depends on `sigma`, `postmodern`, `cl-migratum`,
`cl-migratum.provider.local-path`, and
`cl-migratum.driver.postmodern-postgresql`.

PostgreSQL 13 or later is required (`gen_random_uuid()` is built in).

## Schema

Tables, all with UUID primary keys:

| Table | What it holds |
| --- | --- |
| `instrument_types` | What an instrument is: stock, cryptocurrency, currency, ... |
| `data_source_kinds` | How we talk to a source: api, website, feed, file, manual |
| `exchanges` | Where it traded: NASDAQ, Binance, Uniswap V3, ... |
| `instruments` | One instrument (identity is the UUID, not a ticker) |
| `tickers` | Abbreviation a source (and optional exchange) uses, with a validity window |
| `candlestick_durations` | `s`/`m`/`h`/`d`/`w`/`M`/`y` plus a PostgreSQL interval |
| `data_sources` | Who told us: CoinGecko, Yahoo Finance, Zapper.fi, ... |
| `data_retrievals` | One fetch from a source, optionally pinned to an exchange |
| `candlesticks` | One OHLC bar, linked to a pair, a duration, and a retrieval |

A **data source** is who told us. An **exchange** is where it traded
(including DEXes; `chain` distinguishes Uniswap V3 on Ethereum vs
Arbitrum). A **market** in the pair-on-a-venue sense is the bar's
numerator/denominator plus the exchange — not a separate table yet.

A ticker is unique as `(data_source, canonical abbreviation, exchange)`
at a given time. CoinGecko's aggregated `BTC` (no exchange) and Yahoo's
`BTC` on NYSE can be different instruments. The same abbreviation at one
source can also mean different instruments in non-overlapping
`valid_from` / `valid_to` windows (old GM vs new GM).

The unique key on `candlesticks` is `(numerator, denominator, duration,
time, retrieval)`.  The same bar from two sources -- or from two retrievals
of the same source -- is two rows.  That is how we keep multiple instances
of the same data and still know where each one came from.

`is_closed` is true when the print was taken after that bar's session
ended.  It defaults to true, so a history fetch is stored as finished.
Pass `:closed` to `ingest-candlesticks` as `nil`, or as a function of the
bar's universal time, to store an open session such as today's crypto
candle.  The library does not decide when the session ends.

## A first session

```lisp
(ql:quickload :candlesticks)
(in-package :candlesticks)

(setf *default-connection-spec*
      '("candlesticks" "candlesticks" "secret" "localhost" :port 5432))

(with-connection nil
  (run-migrations *default-connection-spec*)
  (ensure-standard-durations)
  (ensure-standard-instrument-types)
  (ensure-standard-data-source-kinds)
  (ensure-standard-exchanges)
  (ingest-candlesticks "BTC" "USD" "d"
                       (list (list (encode-universal-time 0 0 0 18 8 2026 0)
                                   25000.0 26000.0 24000.0 25500.0 100.0))
                       :source-name "CoinGecko"
                       :source-kind "api"
                       :numerator-name "Bitcoin"
                       :numerator-type "cryptocurrency"
                       :denominator-name "US Dollar"
                       :denominator-type "currency")
  (candlestick-close-prices (get-candlesticks "BTC" "USD" "d")))
```

`with-connection` takes a Postmodern spec -- a list
`postmodern:connect` can be applied to.  Nesting it gives you several
independent connections, which is how you work against more than one
database at a time.

## Testing

Specs are `sigma/behave` `behavior` / `should` forms, the same style as
[sigma](https://github.com/cgore/sigma).  Pure helpers run at load time.
`asdf:test-system` reloads the sources so those assertions run again.

```lisp
(asdf:test-system :candlesticks)
```

To also hit a live PostgreSQL server, set two environment variables and
run `asdf:test-system` again:

```
CANDLESTICKS_LIVE_TESTS=1
CANDLESTICKS_LIVE_DB="database user password host port"
```

The live suite creates a throwaway schema, runs the migrations, exercises
the whole API (including provenance, duplicate ingestion, and nested
connections), and drops the schema when it is done.

## License

Copyright (c) 2026, Christopher Mark Gore,  
Soli Deo Gloria,  
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of Christopher Mark Gore nor the names of other contributors may be used to endorse or promote products derived from this software without specific prior written permission.

**THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS *"AS IS"* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.**
