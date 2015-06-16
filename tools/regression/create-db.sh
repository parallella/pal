#!/bin/bash
DB=pal.db

cat << EOF | sqlite3 $DB
CREATE TABLE IF NOT EXISTS report (
    commit_date INTEGER, -- UNIX epoch
    commit_sha TEXT,
    platform TEXT,
    file TEXT,
    symbol TEXT,
    cflags TEXT,
    type TEXT,
    size INTEGER,
    PRIMARY KEY (commit_date, commit_sha, platform, file, symbol)
);
EOF
