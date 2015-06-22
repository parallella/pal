#!/bin/bash
set -e

# Track reports + db in separate repository. One branch.

# So first save current branch, checkout master, copy tools to temp dir,
# checkout reports branch, copy db,
# checkout current branch again, create reports,

usage() {
    echo Usage: $0 [RANGE] >&2
    echo >&2
    echo Create HTML report for PLATFORM >&2
    echo Example: $0 x86_64 >&2
    exit 1
}

[ $# = 0 -o $# = 1 ] || usage
range=$1


git_dirty_str() {
    # ??? TODO: Won't work if something was added to index ?
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "-dirty" || true
}

git_dirty() {
    # ??? TODO: Won't work if something was added to index ?
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]
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

if [ "x${PAL_TOOLS}" = "x" ]; then
    PAL_TOOLS=$(mktemp -d)
    export PAL_TOOLS
    cp -rf $top_srcdir/tools/* ${PAL_TOOLS}/
    created_pal_tools="yes"
fi

# Keep track of original branch
orig_branch=$(git symbolic-ref --short HEAD 2>/dev/null)
sha=$(git rev-parse --verify HEAD)$(git_dirty_str)

# TODO: Check if PAL_REPORTS is set, then we don't need to clone.
reportsdir=$(mktemp -d)
git clone git@github.com:/parallella/pal-stats.git $reportsdir
export PAL_DB=$reportsdir/pal.db

echo Building x86_64
$PAL_TOOLS/regression/all.sh x86_64              $range && build_x86_64="ok"
echo Building arm
$PAL_TOOLS/regression/all.sh arm-linux-gnueabihf $range && build_arm="ok"
echo Building epiphany
$PAL_TOOLS/regression/all.sh epiphany-elf        $range && build_epiphany="ok"

echo Generating reports
mkdir -p $reportsdir/$orig_branch
$PAL_TOOLS/regression/extract.sh x86_64              $range > $reportsdir/$orig_branch/codesize.x86_64.html
$PAL_TOOLS/regression/extract.sh arm-linux-gnueabihf $range > $reportsdir/$orig_branch/codesize.arm.html
$PAL_TOOLS/regression/extract.sh epiphany-elf        $range > $reportsdir/$orig_branch/codesize.epiphany.html

# Optimize DB size
echo "VACUUM;" | sqlite3 $PAL_DB

# Push changes
cd $reportsdir
git add pal.db
git add $orig_branch
git commit -m"${orig_branch}:${sha}"
git push

cd $top_srcdir
rm -rf $reportsdir

if [ "x${created_pal_tools}" = "xyes" ]; then
    rm -rf ${PAL_TOOLS}
fi
