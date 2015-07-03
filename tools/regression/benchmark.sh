#!/bin/bash
# Must be called from inside git repository
# TODO: Support epiphany

set -e

usage() {
    echo "Usage: $0 [PLATFORM]" >&2
    echo >&2
    echo Reports benchmark results. >&2
    exit 1
}

[ $# == 0  -o $# == 1 ] || usage

if ! which sqlite3 >/dev/null; then
    echo This tool needs sqlite3 >&2
    exit 1
fi

if ! which gawk >/dev/null; then
    echo This tool needs gawk >&2
    exit 1
fi

git_dirty_str() {
    # ??? TODO: Won't work if something was added to index ?
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "-dirty" || true
}

build_arch=$(uname -m)

platform=$1
if [ "x$platform" != "x" ]; then
    host_str="--host=${platform}"
else
    platform=$build_arch
fi

sha=$(git rev-parse --verify HEAD)$(git_dirty_str)
commit_date=$(git show -s --format="%ct" HEAD)

top_srcdir=$(git rev-parse --show-toplevel)
builddir=$(mktemp -d /tmp/palXXXXXXXX)

if [ "x${PAL_TOOLS}" = "x" ]; then
    PAL_TOOLS=$(mktemp -d)
    export PAL_TOOLS
    cp -rf $top_srcdir/tools/* ${PAL_TOOLS}/
    created_pal_tools="yes"
fi

if [ "x${PAL_REPORTS}" = "x" ]; then
    PAL_REPORTS=$(mktemp -d)
    git clone git@github.com:/parallella/pal-stats.git $PAL_REPORTS
    export PAL_REPORTS
    export PAL_DB=$PAL_REPORTS/pal.db

    # Create/update Database if necessary
    $PAL_TOOLS/regression/create-db.sh
    created_pal_reports="yes"
fi

# Bootstrap
(cd $top_srcdir && ./bootstrap >$builddir/boostrap.log 2>&1)

# Build in builddir
cd $builddir
$top_srcdir/configure ${host_str} >> build.log 2>&1

# Compile src and bench
(cd src && make -j -k >> build.log 2>&1 || true)
(cd benchmark && make -j -k >> build.log 2>&1 || true)

# Run benchmark when not cross compiling
platform_short=$(echo $platform | cut -f1 -d"-")
if [ "x${platform_short}" = "x${build_arch}" ]; then
    if [ -e "${builddir}/benchmark/math/bench_all" ]; then
        bench_res=$(mktemp)
        ${builddir}/benchmark/math/bench_all | gawk -F"," '
        {
            if (substr($0, 0, 1) == ";") {
                next;
            }

            fn=$1;
            result=$3/$2;
            unit="ns";

            printf("%s,%s,%s,%s,%s,%f,%s\n", ts, sha, platform, fn, cflags, result, unit);
        }' ts=$commit_date sha=$sha cflags=$CFLAGS platform=$platform > $bench_res
        (
            echo ".mode csv"
            echo ".import $bench_res benchmarks"
        ) | sqlite3 ${PAL_DB}
        rm $bench_res
    fi
fi

cd $PAL_REPORTS
git add -u
git commit -m"Benchmarks for $platform $sha"


cd $top_srcdir
rm -rf $builddir

if [ "x${created_pal_tools}" = "xyes" ]; then
    rm -rf ${PAL_TOOLS}
fi

if [ "x${created_pal_reports}" = "xyes" ]; then
    cd $PAL_REPORTS
    git push
    cd $top_srcdir
    rm -rf $PAL_REPORTS
fi
