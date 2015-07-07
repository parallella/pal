#!/bin/bash
PAL_DB=${PAL_DB:-pal.db}

cat << EOF | sqlite3 $PAL_DB
CREATE TABLE IF NOT EXISTS report (
    commit_date INTEGER, -- UNIX epoch
    commit_sha TEXT,
    platform TEXT,
    file TEXT,
    symbol TEXT,
    cflags TEXT,
    type TEXT,
    size INTEGER,
    PRIMARY KEY (commit_date, commit_sha, platform, file, symbol, cflags)
);
CREATE TABLE IF NOT EXISTS benchmarks (
    commit_date INTEGER, -- UNIX epoch
    commit_sha TEXT,
    platform TEXT,
    function TEXT,
    cflags TEXT,
    result REAL,
    unit TEXT,
    PRIMARY KEY (commit_date, commit_sha, platform, function, cflags)
);
EOF
