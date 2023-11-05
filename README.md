# PostgreSQL Useful Queries

# Overview

This README.md file provides a collection of SQL queries for PostgreSQL database administrators to monitor and maintain
their databases. It includes queries for retrieving table indexes, partitions, and various data integrity checks.

# Table of Contents

- [Basics](#basics)
    - [Get indexes of tables](#get-indexes-of-tables)
    - [Get partitions of tables](#get-partitions-of-tables)
- [Data Integrity](#data-integrity)
    - [Cache Hit Ratio](#cache-hit-ratio)
    - [Anomalies](#anomalies)
    - [Database Sizes](#database-sizes)
    - [Schemas size](#schemas-size)
    - [Table Sizes](#table-sizes)
    - [Another Table Sizes Query](#another-table-sizes-query)
    - [Unused Indexes](#unused-indexes)
    - [Write Activity(index usage)](#write-activityindex-usage)
    - [Does table needs an Index](#does-table-needs-an-index)
    - [Index % usage](#index--usage)
    - [How many indexes are in cache](#how-many-indexes-are-in-cache)
    - [Dirty Pages](#dirty-pages)
    - [Sequential Scans](#sequential-scans)
    - [Checkpoints](#checkpoints)
- [Activity](#activity)
    - [Show running queries](#show-running-queries)
    - [Kill running query](#kill-running-query)
    - [Kill idle query](#kill-idle-query)
    - [Check locks](#check-locks)
    - [Get Running Queries (And Lock statuses)](#get-running-queries-and-lock-statuses)
    - [Most CPU intensive queries (PGSQL v9.4)](#most-cpu-intensive-queries-pgsql-v94)
    - [Most time consuming queries (PGSQL v9.4)](#most-time-consuming-queries-pgsql-v94)
    - [Maximum transaction age](#maximum-transaction-age)
    - [Bad xacts](#bad-xacts)
    - [Waiting Clients](#waiting-clients)
    - [Waiting Connections for a lock](#waiting-connections-for-a-lock)
    - [Connections](#connections)
    - [User Connections Ratio](#user-connections-ratio)
    - [Average Statement Exec Time](#average-statement-exec-time)
    - [Most writing (to shared_buffers) queries](#most-writing-to-shared_buffers-queries)
    - [Block Read Time](#block-read-time)
- [Vacuuming](#vacuuming)
    - [Vacuum Command](#vacuum-command)
    - [Auto vacuum](#auto-vacuum)
    - [Last Vacuum and Analyze time](#last-vacuum-and-analyze-time)
    - [Total number of dead tuples need to be vacuumed per table](#total-number-of-dead-tuples-need-to-be-vacuumed-per-table)
    - [Total number of dead tuples need to be vacuumed in DB](#total-number-of-dead-tuples-need-to-be-vacuumed-in-db)
    - [Show Table Bloats](#show-table-bloats)



# Basics

------------

## Get indexes of tables

```postgresql

select t.relname                  as table_name,
       i.relname                  as index_name,
       string_agg(a.attname, ',') as column_name
from pg_class t,
     pg_class i,
     pg_index ix,
     pg_attribute a
where t.oid = ix.indrelid
  and i.oid = ix.indexrelid
  and a.attrelid = t.oid
  and a.attnum = ANY (ix.indkey)
  and t.relkind = 'r'
  and t.relname not like 'pg_%'
group by t.relname,
         i.relname
order by t.relname,
         i.relname;

```

## Get partitions of tables

```postgresql
SELECT nmsp_parent.nspname AS parent_schema,
       parent.relname      AS parent,
       nmsp_child.nspname  AS child_schema,
       child.relname       AS child
FROM pg_inherits
         JOIN pg_class parent ON pg_inherits.inhparent = parent.oid
         JOIN pg_class child ON pg_inherits.inhrelid = child.oid
         JOIN pg_namespace nmsp_parent ON nmsp_parent.oid = parent.relnamespace
         JOIN pg_namespace nmsp_child ON nmsp_child.oid = child.relnamespace
WHERE parent.relname = '<table_name>'
ORDER BY child;
```

# Data Integrity

--------------------

#### Cache Hit Ratio

- Ideally, the hit ratio should be greater than 90%.

````postgresql
select sum(blks_hit) * 100 / sum(blks_hit + blks_read) as hit_ratio
from pg_stat_database;
````

#### Anomalies

- c_commit_ratio should be > 95%
- c_rollback_ratio should be < 5%
- deadlocks should be close to 0
- conflicts should be close to 0
- temp_files and temp_bytes watch out for them

````postgresql
select datname,
       (xact_commit100) / nullif(xact_commit + xact_rollback, 0)   as c_commit_ratio,
       (xact_rollback100) / nullif(xact_commit + xact_rollback, 0) as c_rollback_ratio,
       deadlocks,
       conflicts,
       temp_files,
       pg_size_pretty(temp_bytes)
from pg_stat_database;
````

#### Database Sizes

````postgresql
select datname, pg_size_pretty(pg_database_size(datname))
from pg_database
order by pg_database_size(datname);
````

#### Schemas size

````postgresql
WITH schemas AS (
    SELECT schemaname                                                                              as name,
           sum(pg_relation_size(quote_ident(schemaname) || '.' || quote_ident(tablename)))::bigint as size
    FROM pg_tables
    GROUP BY schemaname
),

     db AS (
         SELECT pg_database_size(current_database()) AS size
     )

SELECT schemas.name,
       pg_size_pretty(schemas.size)                      as absolute_size,
       schemas.size::float / (SELECT size FROM db) * 100 as relative_size
FROM schemas;
````

#### Table Sizes

````postgresql
select relname,
       pg_size_pretty(pg_total_relation_size(relname::regclass))                                       as full_size,
       pg_size_pretty(pg_relation_size(relname::regclass))                                             as table_size,
       pg_size_pretty(pg_total_relation_size(relname::regclass) - pg_relation_size(relname::regclass)) as index_size
from pg_stat_user_tables
order by pg_total_relation_size(relname::regclass) desc
limit 10;
````

#### Another Table Sizes Query

````postgresql
SELECT nspname || '.' || relname AS "relation", pg_size_pretty(pg_total_relation_size(C.oid)) AS "total_size"
FROM pg_class C
         LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
WHERE nspname NOT IN ('pg_catalog', 'information_schema')
  AND C.relkind <> 'i'
  AND nspname !~ '^pg_toast'
ORDER BY pg_total_relation_size(C.oid) DESC;
````

#### Unused Indexes

- idx_scan should not be = 0

````postgresql
select *
from pg_stat_all_indexes
where idx_scan = 0;
````

#### Write Activity(index usage)

- hot_rate should be close to 100

````postgresql
select s.relname,
       pg_size_pretty(pg_relation_size(relid)),
       coalesce(n_tup_ins, 0) + 2 * coalesce(n_tup_upd, 0) - coalesce(n_tup_hot_upd, 0) +
       coalesce(n_tup_del, 0)                                                                  AS total_writes,
       (coalesce(n_tup_hot_upd, 0)::float * 100 /
        (case when n_tup_upd > 0 then n_tup_upd else 1 end)::float)::numeric(10, 2)            AS hot_rate,
       (select v[1] FROM regexp_matches(reloptions::text, E'fillfactor=(d+)') as r(v) limit 1) AS fillfactor
from pg_stat_all_tables s
         join pg_class c ON c.oid = relid
order by total_writes desc
limit 50;
````

#### Does table needs an Index

````postgresql
SELECT relname,
       seq_scan - idx_scan                 AS too_much_seq,
       CASE WHEN seq_scan - idx_scan > 0 THEN 'Missing Index?' ELSE 'OK' END,
       pg_relation_size(relname::regclass) AS rel_size,
       seq_scan,
       idx_scan
FROM pg_stat_all_tables
WHERE schemaname = 'public'
  AND pg_relation_size(relname::regclass) > 80000
ORDER BY too_much_seq DESC;
````

#### Index % usage

````postgresql
SELECT relname, 100 * idx_scan / (seq_scan + idx_scan) percent_of_times_index_used, n_live_tup rows_in_table
FROM pg_stat_user_tables
ORDER BY n_live_tup DESC;
````

#### How many indexes are in cache

````postgresql
SELECT sum(idx_blks_read)                                           as idx_read,
       sum(idx_blks_hit)                                            as idx_hit,
       (sum(idx_blks_hit) - sum(idx_blks_read)) / sum(idx_blks_hit) as ratio
FROM pg_statio_user_indexes;
````

#### Dirty Pages

- maxwritten_clean and buffers_backend_fsyn better be = 0

````postgresql
select buffers_clean, maxwritten_clean, buffers_backend_fsync
from pg_stat_bgwriter;
````

#### Sequential Scans

- seq_tup_avg should be < 1000

````postgresql
select relname,
       pg_size_pretty(pg_relation_size(relname::regclass)) as size,
       seq_scan,
       seq_tup_read,
       seq_scan / seq_tup_read                             as seq_tup_avg
from pg_stat_user_tables
where seq_tup_read > 0
order by 3, 4 desc
limit 5;
````

#### Checkpoints

````postgresql
select 'bad' as checkpoints
from pg_stat_bgwriter
where checkpoints_req > checkpoints_timed;
````

--------------

# Activity

--------------

#### Show running queries

````postgresql
SELECT pid, state, age(clock_timestamp(), query_start), usename, query
FROM pg_stat_activity
WHERE query != '<IDLE>'
  AND query NOT ILIKE '%pg_stat_activity%'
ORDER BY query_start desc;
````

#### Kill running query

````postgresql
SELECT pg_cancel_backend(procpid);
````

#### Kill idle query

````postgresql
SELECT pg_terminate_backend(procpid);
````

#### Check locks

```postgresql
SELECT blocked_locks.pid         AS blocked_pid,
       blocked_activity.usename  AS blocked_user,
       blocking_locks.pid        AS blocking_pid,
       blocking_activity.usename AS blocking_user,
       blocked_activity.query    AS blocked_statement,
       blocking_activity.query   AS current_statement_in_blocking_process
FROM pg_catalog.pg_locks blocked_locks
         JOIN pg_catalog.pg_stat_activity blocked_activity ON blocked_activity.pid = blocked_locks.pid
         JOIN pg_catalog.pg_locks blocking_locks
              ON blocking_locks.locktype = blocked_locks.locktype
                  AND blocking_locks.database IS NOT DISTINCT FROM blocked_locks.database
                  AND blocking_locks.relation IS NOT DISTINCT FROM blocked_locks.relation
                  AND blocking_locks.page IS NOT DISTINCT FROM blocked_locks.page
                  AND blocking_locks.tuple IS NOT DISTINCT FROM blocked_locks.tuple
                  AND blocking_locks.virtualxid IS NOT DISTINCT FROM blocked_locks.virtualxid
                  AND blocking_locks.transactionid IS NOT DISTINCT FROM blocked_locks.transactionid
                  AND blocking_locks.classid IS NOT DISTINCT FROM blocked_locks.classid
                  AND blocking_locks.objid IS NOT DISTINCT FROM blocked_locks.objid
                  AND blocking_locks.objsubid IS NOT DISTINCT FROM blocked_locks.objsubid
                  AND blocking_locks.pid != blocked_locks.pid

         JOIN pg_catalog.pg_stat_activity blocking_activity ON blocking_activity.pid = blocking_locks.pid
WHERE NOT blocked_locks.granted;
```

```postgresql
SELECT a.datname,
       l.relation::regclass,
       l.transactionid,
       l.mode,
       l.GRANTED,
       a.usename,
       a.query,
       a.query_start,
       age(now(), a.query_start) AS "age",
       a.pid
FROM pg_stat_activity a
         JOIN pg_locks l ON l.pid = a.pid
ORDER BY a.query_start;
```

#### Get Running Queries (And Lock statuses)

```postgresql
SELECT S.pid,
       age(clock_timestamp(), query_start),
       usename,
       query,
       L.mode,
       L.locktype,
       L.granted
FROM pg_stat_activity S
         inner join pg_locks L on S.pid = L.pid
order by L.granted, L.pid DESC
```

#### Most CPU intensive queries (PGSQL v9.4)

```postgresql
SELECT substring(query, 1, 50)                                                  AS short_query,
       round(total_time::numeric, 2)                                            AS total_time,
       calls,
       rows,
       round(total_time::numeric / calls, 2)                                    AS avg_time,
       round((100 * total_time / sum(total_time::numeric) OVER ())::numeric, 2) AS percentage_cpu
FROM pg_stat_statements
ORDER BY total_time DESC
LIMIT 20;
```

#### Most time consuming queries (PGSQL v9.4)

```postgresql
SELECT substring(query, 1, 100)                                                 AS short_query,
       round(total_time::numeric, 2)                                            AS total_time,
       calls,
       rows,
       round(total_time::numeric / calls, 2)                                    AS avg_time,
       round((100 * total_time / sum(total_time::numeric) OVER ())::numeric, 2) AS percentage_cpu
FROM pg_stat_statements
ORDER BY avg_time DESC
LIMIT 20;
```

#### Maximum transaction age

-- Long-running transactions are bad because they prevent Postgres from vacuuming old data. This causes database bloat
and, in extreme circumstances, shutdown due to transaction ID (xid) wraparound. Transactions should be kept as short as
possible, ideally less than a minute.

```postgresql
select client_addr,
       usename,
       datname,
       clock_timestamp() - xact_start  as xact_age,
       clock_timestamp() - query_start as query_age,
       query
from pg_stat_activity
order by xact_start, query_start;
```

#### Bad xacts

```postgresql
select *
from pg_stat_activity
where state in ('idle in transaction', 'idle in transaction (aborted)');
```

#### Waiting Clients

```postgresql
select *
from pg_stat_activity
where waiting;
```

#### Waiting Connections for a lock

```postgresql
SELECT count(distinct pid)
FROM pg_locks
WHERE granted = false;
```

### Connections

```postgresql
select client_addr, usename, datname, count(*)
from pg_stat_activity
group by 1, 2, 3
order by 4 desc;
```

### User Connections Ratio

```postgresql
select count(*) * 100 / (select current_setting('max_connections')::int)
from pg_stat_activity;
```

### Average Statement Exec Time

```postgresql
select (sum(total_time) / sum(calls))::numeric(6, 3)
from pg_stat_statements;
```

### Most writing (to shared_buffers) queries

```postgresql
select query, shared_blks_dirtied
from pg_stat_statements
where shared_blks_dirtied > 0
order by 2 desc;
```

### Block Read Time

```postgresql
select *
from pg_stat_statements
where blk_read_time <> 0
order by blk_read_time desc;
```

---------------

# Vacuuming

---------------

#### Vacuum Command

````postgresql
VACUUM (VERBOSE, ANALYZE);
````

````postgresql
VACUUM FULL ANALYSE 'table_name';
````

#### Auto vacuum

```postgresql
SELECT *
FROM pg_settings
WHERE name LIKE 'autovacuum%'
```

#### Last Vacuum and Analyze time

```postgresql
select relname, last_vacuum, last_autovacuum, last_analyze, last_autoanalyze
from pg_stat_user_tables;
```

#### Total number of dead tuples need to be vacuumed per table

```postgresql
select n_dead_tup, schemaname, relname
from pg_stat_all_tables;
```

#### Total number of dead tuples need to be vacuumed in DB

```postgresql
select sum(n_dead_tup)
from pg_stat_all_tables;
```

#### Show Table Bloats

```postgresql
with foo as (
    SELECT schemaname,
           tablename,
           hdr,
           ma,
           bs,
           SUM((1 - null_frac) * avg_width) AS datawidth,
           MAX(null_frac)                   AS maxfracsum,
           hdr + (
               SELECT 1 + COUNT(*) / 8
               FROM pg_stats s2
               WHERE null_frac <> 0
                 AND s2.schemaname = s.schemaname
                 AND s2.tablename = s.tablename
           )                                AS nullhdr
    FROM pg_stats s,
         (
             SELECT (SELECT current_setting('block_size')::NUMERIC)                            AS bs,
                    CASE WHEN SUBSTRING(v, 12, 3) IN ('8.0', '8.1', '8.2') THEN 27 ELSE 23 END AS hdr,
                    CASE WHEN v ~ 'mingw32' THEN 8 ELSE 4 END                                  AS ma
             FROM (SELECT version() AS v) AS foo
         ) AS constants
    GROUP BY 1, 2, 3, 4, 5
),
     rs as (
         SELECT ma,
                bs,
                schemaname,
                tablename,
                (datawidth + (hdr + ma - (CASE WHEN hdr % ma = 0 THEN ma ELSE hdr % ma END)))::NUMERIC     AS datahdr,
                (maxfracsum * (nullhdr + ma - (CASE WHEN nullhdr % ma = 0 THEN ma ELSE nullhdr % ma END))) AS nullhdr2
         FROM foo
     ),
     sml as (
         SELECT schemaname,
                tablename,
                cc.reltuples,
                cc.relpages,
                bs,
                CEIL((cc.reltuples * ((datahdr + ma -
                                       (CASE WHEN datahdr % ma = 0 THEN ma ELSE datahdr % ma END)) + nullhdr2 + 4)) /
                     (bs - 20::FLOAT))    AS otta,
                COALESCE(c2.relname, '?') AS iname,
                COALESCE(c2.reltuples, 0) AS ituples,
                COALESCE(c2.relpages, 0)  AS ipages,
                COALESCE(CEIL((c2.reltuples * (datahdr - 12)) / (bs - 20::FLOAT)),
                         0)               AS iotta -- very rough approximation, assumes all cols
         FROM rs
                  JOIN pg_class cc ON cc.relname = rs.tablename
                  JOIN pg_namespace nn
                       ON cc.relnamespace = nn.oid AND nn.nspname = rs.schemaname AND nn.nspname <> 'information_schema'
                  LEFT JOIN pg_index i ON indrelid = cc.oid
                  LEFT JOIN pg_class c2 ON c2.oid = i.indexrelid
     )

SELECT current_database(),
       schemaname,
       tablename, /*reltuples::bigint, relpages::bigint, otta,*/
       ROUND((CASE WHEN otta = 0 THEN 0.0 ELSE sml.relpages::FLOAT / otta END)::NUMERIC, 1)           AS tbloat,
       CASE WHEN relpages < otta THEN 0 ELSE bs * (sml.relpages - otta)::BIGINT END                   AS wastedbytes,
       iname, /*ituples::bigint, ipages::bigint, iotta,*/
       ROUND((CASE WHEN iotta = 0 OR ipages = 0 THEN 0.0 ELSE ipages::FLOAT / iotta END)::NUMERIC, 1) AS ibloat,
       CASE WHEN ipages < iotta THEN 0 ELSE bs * (ipages - iotta) END                                 AS wastedibytes
FROM sml
ORDER BY wastedbytes DESC
```

You should be looking at:

tbloat: table bloat, ratio between what it current is, and what it can be optimized to. wastedbytes: number of bytes
wasted ibloat & wastedibytes: same as above, but for indexes. When you see a table with high bloats, then consider
running VACUUM ANALYZE on it.

```postgresql
select table_schema,
       table_name,
       free_percent,
       pg_size_pretty(free_space)                    AS space_free,
       pg_size_pretty(pg_relation_size(quoted_name)) AS total_size
from (
         select table_schema,
                table_name,
                quoted_name,
                space_stats.approx_free_percent AS free_percent,
                space_stats.approx_free_space   AS free_space
         from (select *,
                      quote_ident(table_schema) || '.' || quote_ident(table_name) AS quoted_name
               from information_schema.tables
               where table_type = 'BASE TABLE'
                 and table_schema not in ('information_schema', 'pg_catalog')
                 and pg_relation_size(quote_ident(table_schema) || '.' || quote_ident(table_name)) > 100000000
              ) t,
              pgstattuple_approx(quoted_name) AS space_stats
     ) t
where free_percent > 20
  AND free_space > 10000000
ORDER BY free_space DESC;
```

```postgresql
WITH constants AS (
    -- define some constants for sizes of things
    -- for reference down the query and easy maintenance
    SELECT current_setting('block_size')::numeric AS bs, 23 AS hdr, 8 AS ma
),
     no_stats AS (
         -- screen out table who have attributes
         -- which dont have stats, such as JSON
         SELECT table_schema,
                table_name,
                n_live_tup::numeric           as est_rows,
                pg_table_size(relid)::numeric as table_size
         FROM information_schema.columns
                  JOIN pg_stat_user_tables as psut
                       ON table_schema = psut.schemaname
                           AND table_name = psut.relname
                  LEFT OUTER JOIN pg_stats
                                  ON table_schema = pg_stats.schemaname
                                      AND table_name = pg_stats.tablename
                                      AND column_name = attname
         WHERE attname IS NULL
           AND table_schema NOT IN ('pg_catalog', 'information_schema')
         GROUP BY table_schema, table_name, relid, n_live_tup
     ),
     null_headers AS (
         -- calculate null header sizes
         -- omitting tables which dont have complete stats
         -- and attributes which aren't visible
         SELECT hdr + 1 + (sum(case when null_frac <> 0 THEN 1 else 0 END) / 8) as nullhdr,
                SUM((1 - null_frac) * avg_width)                                as datawidth,
                MAX(null_frac)                                                  as maxfracsum,
                schemaname,
                tablename,
                hdr,
                ma,
                bs
         FROM pg_stats
                  CROSS JOIN constants
                  LEFT OUTER JOIN no_stats
                                  ON schemaname = no_stats.table_schema
                                      AND tablename = no_stats.table_name
         WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
           AND no_stats.table_name IS NULL
           AND EXISTS(SELECT 1
                      FROM information_schema.columns
                      WHERE schemaname = columns.table_schema
                        AND tablename = columns.table_name)
         GROUP BY schemaname, tablename, hdr, ma, bs
     ),
     data_headers AS (
         -- estimate header and row size
         SELECT ma,
                bs,
                hdr,
                schemaname,
                tablename,
                (datawidth + (hdr + ma - (case when hdr % ma = 0 THEN ma ELSE hdr % ma END)))::numeric     AS datahdr,
                (maxfracsum * (nullhdr + ma - (case when nullhdr % ma = 0 THEN ma ELSE nullhdr % ma END))) AS nullhdr2
         FROM null_headers
     ),
     table_estimates AS (
         -- make estimates of how large the table should be
         -- based on row and page size
         SELECT schemaname,
                tablename,
                bs,
                reltuples::numeric             as est_rows,
                relpages * bs                  as table_bytes,
                CEIL((reltuples *
                      (datahdr + nullhdr2 + 4 + ma -
                       (CASE
                            WHEN datahdr % ma = 0
                                THEN ma
                            ELSE datahdr % ma END)
                          ) / (bs - 20))) * bs AS expected_bytes,
                reltoastrelid
         FROM data_headers
                  JOIN pg_class ON tablename = relname
                  JOIN pg_namespace ON relnamespace = pg_namespace.oid
             AND schemaname = nspname
         WHERE pg_class.relkind = 'r'
     ),
     estimates_with_toast AS (
         -- add in estimated TOAST table sizes
         -- estimate based on 4 toast tuples per page because we dont have
         -- anything better.  also append the no_data tables
         SELECT schemaname,
                tablename,
                TRUE                                                           as can_estimate,
                est_rows,
                table_bytes + (coalesce(toast.relpages, 0) * bs)               as table_bytes,
                expected_bytes + (ceil(coalesce(toast.reltuples, 0) / 4) * bs) as expected_bytes
         FROM table_estimates
                  LEFT OUTER JOIN pg_class as toast
                                  ON table_estimates.reltoastrelid = toast.oid
                                      AND toast.relkind = 't'
     ),
     table_estimates_plus AS (
-- add some extra metadata to the table data
-- and calculations to be reused
-- including whether we cant estimate it
-- or whether we think it might be compressed
         SELECT current_database()      as databasename,
                schemaname,
                tablename,
                can_estimate,
                est_rows,
                CASE
                    WHEN table_bytes > 0
                        THEN table_bytes::NUMERIC
                    ELSE NULL::NUMERIC END
                                        AS table_bytes,
                CASE
                    WHEN expected_bytes > 0
                        THEN expected_bytes::NUMERIC
                    ELSE NULL::NUMERIC END
                                        AS expected_bytes,
                CASE
                    WHEN expected_bytes > 0 AND table_bytes > 0
                        AND expected_bytes <= table_bytes
                        THEN (table_bytes - expected_bytes)::NUMERIC
                    ELSE 0::NUMERIC END AS bloat_bytes
         FROM estimates_with_toast
         UNION ALL
         SELECT current_database() as databasename,
                table_schema,
                table_name,
                FALSE,
                est_rows,
                table_size,
                NULL::NUMERIC,
                NULL::NUMERIC
         FROM no_stats
     ),
     bloat_data AS (
         -- do final math calculations and formatting
         select current_database()                             as databasename,
                schemaname,
                tablename,
                can_estimate,
                table_bytes,
                round(table_bytes / (1024 ^ 2)::NUMERIC, 3)    as table_mb,
                expected_bytes,
                round(expected_bytes / (1024 ^ 2)::NUMERIC, 3) as expected_mb,
                round(bloat_bytes * 100 / table_bytes)         as pct_bloat,
                round(bloat_bytes / (1024::NUMERIC ^ 2), 2)    as mb_bloat,
                table_bytes,
                expected_bytes,
                est_rows
         FROM table_estimates_plus
     )
-- filter output for bloated tables
SELECT databasename,
       schemaname,
       tablename,
       can_estimate,
       est_rows,
       pct_bloat,
       mb_bloat,
       table_mb
FROM bloat_data
-- this where clause defines which tables actually appear
-- in the bloat chart
-- example below filters for tables which are either 50%
-- bloated and more than 20mb in size, or more than 25%
-- bloated and more than 1GB in size
WHERE (pct_bloat >= 50 AND mb_bloat >= 20)
   OR (pct_bloat >= 25 AND mb_bloat >= 1000)
ORDER BY pct_bloat DESC;
```
