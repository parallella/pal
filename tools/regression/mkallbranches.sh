#!/bin/bash
set -e

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

git fetch

if [ "x${PAL_TOOLS}" = "x" ]; then
    PAL_TOOLS=$(mktemp -d)
    export PAL_TOOLS
    cp -rf $top_srcdir/tools/* ${PAL_TOOLS}/
    created_pal_tools="yes"
fi

# Keep track of original branch
orig_branch=$(git symbolic-ref --short HEAD 2>/dev/null)

echo Building for current branch: $orig_branch
$PAL_TOOLS/regression/mkallreports.sh


# TODO: Git remote add ... && fetch instr
openprs=$(curl --silent https://api.github.com/repos/parallella/pal/pulls?state=open | grep '"number"' | cut -f2 -d":" | tr -d " ," | sort -g)
#openprs=$(git branch -r --no-merged master | tr -d " " | grep "^origin/pr" | sort -g)

for n in $openprs; do
    echo Building open pull request: $n
    git checkout origin/pr/$n -b pr-$n
    range=$(git merge-base master pr-$n)~..pr-$n
    $PAL_TOOLS/regression/mkallreports.sh $range
    git reset --hard
    git checkout $orig_branch
    git branch -d pr-$n
done

if [ "x${created_pal_tools}" = "xyes" ]; then
    rm -rf ${PAL_TOOLS}
fi
