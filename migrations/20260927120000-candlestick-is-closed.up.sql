-- A bar is closed when the print was taken after its session ended.
-- Existing rows are closed: they were stored as finished history.

alter table candlesticks
    add column is_closed boolean not null default true;
