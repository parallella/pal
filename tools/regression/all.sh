#!/bin/bash
set -e

DB=pal.db

usage() {
    echo Usage: $0 PLATFORM >/dev/stderr
    echo >/dev/stderr
    echo Reports symbol size for all compilation units in a directory. >/dev/stderr
    exit 1
}

[ $# == 1 ] || usage
platform=$1

git_dirty() {
    # ??? TODO: Won't work if something was added to index ?
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]
}

already_built() {
    platform=$1
    sha=$2

    in_db=$(echo "select \"yes\" from report where platform=\"${platform}\" and commit_sha=\"${sha}\" LIMIT 1;" | sqlite3 ${DB})

    [ "x${in_db}" = "xyes" ]
}

if git_dirty; then
    echo Git index is dirty. Please stash your changes first. >&2
    exit 1
fi

if ! which sqlite3 >/dev/null; then
    echo This tool needs sqlite3 >&2
    exit 1
fi

if ! which gawk >/dev/null; then
    echo This tool needs gawk >&2
    exit 1
fi

top_srcdir=$(git rev-parse --show-toplevel)
cd $top_srcdir

# Copy tools outside of
toolsdir=$(mktemp -d)
cp -rf tools/* $toolsdir/

# Create Database if necessary
$toolsdir/regression/create-db.sh

# All commits on current branch (only follow first parent)
all=$(git log --oneline HEAD --first-parent --format="%H" --reverse)
logfile=$(mktemp)

# Keep track of original branch
orig_branch=$(git symbolic-ref --short HEAD 2>/dev/null)

# Create temporary branch
tmp_branch=regression-test-$(mktemp -u XXXX)
git checkout -b $tmp_branch >/dev/stderr

echo logfile=${logfile}
echo toolsdir=${toolsdir}
for c in $all; do
    git reset --hard $c
    [ -e src -a -e configure.ac ] || continue
    already_built $platform $c && continue
    echo "Building $c" >/dev/stderr
    if $toolsdir/regression/log-code-size.sh $platform > $logfile; then
        # TODO: Perform insert as transaction
        (echo ".mode csv" && echo ".import $logfile report") | sqlite3 ${DB}
    else
        echo Building $c failed
    fi
done

git checkout $orig_branch
git branch -d $tmp_branch
rm -rf $toolsdir
rm -rf $one

