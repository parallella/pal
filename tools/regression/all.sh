#!/bin/bash
set -e
PAL_DB=${PAL_DB:-pal.db}

# First commit that produce any stats
# This is so we don't have to rebuild the first commits every time.
FIRST_COMMIT=521aa636a374c1d8e83df927d50d9a542537362b

usage() {
    echo "Usage: $0 PLATFORM [RANGE]" >&2
    echo >&2
    echo Reports symbol size for all compilation units in a directory. >&2
    exit 1
}

[ $# == 1 -o $# == 2 ] || usage
platform=$1

if [ "x$2" = "x" ]; then
    range="${FIRST_COMMIT}^..HEAD"
else
    range=$2
fi

git_dirty() {
    # ??? TODO: Won't work if something was added to index ?
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]
}

already_built() {
    platform=$1
    sha=$2

    in_db=$(echo "select \"yes\" from report where platform=\"${platform}\" and commit_sha=\"${sha}\" and cflags=\"${CFLAGS}\" LIMIT 1;" | sqlite3 ${PAL_DB})

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
if [ "x${PAL_TOOLS}" = "x" ]; then
    PAL_TOOLS=$(mktemp -d)
    export PAL_TOOLS
    cp -rf $top_srcdir/tools/* ${PAL_TOOLS}/
    created_pal_tools="yes"
fi

# Create Database if necessary
$PAL_TOOLS/regression/create-db.sh

# All commits on current branch (only follow first parent)
all=$(git log --oneline $range --first-parent --format="%H" --reverse)
logfile=$(mktemp)

# Keep track of original branch
orig_branch=$(git symbolic-ref --short HEAD 2>/dev/null)

# Create temporary branch
tmp_branch=regression-test-$(mktemp -u XXXX)
git checkout -b $tmp_branch >&2

for c in $all; do
    already_built $platform $c && continue
    git reset --hard $c
    [ -e src -a -e configure.ac ] || continue
    echo "Building $c" >&2
    if $PAL_TOOLS/regression/log-code-size.sh $platform > $logfile; then
        # TODO: Perform insert as transaction
        (echo ".mode csv" && echo ".import $logfile report") | sqlite3 ${PAL_DB}
    else
        echo Building $c failed
    fi
done

git checkout $orig_branch
git branch -d $tmp_branch
rm -rf $logfile

if [ "x${created_pal_tools}" = "xyes" ]; then
    rm -rf ${PAL_TOOLS}
fi
