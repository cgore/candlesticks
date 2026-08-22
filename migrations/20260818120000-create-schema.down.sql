-- Revert the Candlesticks initial schema.

drop table if exists candlesticks;
--;;
drop table if exists data_retrievals;
--;;
drop table if exists candlestick_durations;
--;;
drop table if exists tickers;
--;;
drop function if exists tickers_no_overlap ();
--;;
drop table if exists data_sources;
--;;
drop table if exists instruments;
--;;
drop table if exists exchanges;
--;;
drop table if exists data_source_kinds;
--;;
drop table if exists instrument_types;
