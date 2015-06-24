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

need_build() {
    # TODO: More flexibility if we do this as a SQL query instead
    # E.g. we can add more CFLAG configurations after first build of branch
    branch=$1
    sha=$2
    ! (
        cd $PAL_REPORTS &&
        git log --oneline --format="%s" | grep -q "^${branch}:${sha}$"
    )
}


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

if [ "x${PAL_REPORTS}" = "x" ]; then
    PAL_REPORTS=$(mktemp -d)
    git clone git@github.com:/parallella/pal-stats.git $PAL_REPORTS
    export PAL_REPORTS
    export PAL_DB=$PAL_REPORTS/pal.db
    created_pal_reports="yes"
fi


# Keep track of original branch
orig_branch=$(git symbolic-ref --short HEAD 2>/dev/null)

# Check out master and pull latest changes
git checkout master
git pull

# Fetch pull requests
# TODO: Git remote add ... && fetch instr
git fetch


create_summary()
{
    sha=$1
    $PAL_TOOLS/regression/summary.sh > $PAL_REPORTS/summary.html
    (
        cd $PAL_REPORTS
        git add pal.db summary.html
        git commit -m"Summary report for $sha"
    )
}

build_w_cflags() {
    platform=$1
    cflags=$2

    echo Building: master $platform \"${cflags}\"

    CFLAGS=${cflags} $PAL_TOOLS/regression/all.sh $platform "master^..master"
}

master_sha=$(git rev-parse master)
echo Building for master branch:
if need_build master $master_sha; then
    # Create code size regression reports
    $PAL_TOOLS/regression/mkallreports.sh

    # Build with more CFLAG configurations for summary report
    build_w_cflags x86_64              "-Os -ffast-math"
    build_w_cflags x86_64              "-O2 -ffast-math"
    build_w_cflags arm-linux-gnueabihf "-Os -ffast-math"
    build_w_cflags arm-linux-gnueabihf "-O2 -ffast-math"
    build_w_cflags epiphany-elf        "-Os -ffast-math -mfp-mode=round-nearest -ffp-contract=fast"
    build_w_cflags epiphany-elf        "-O2 -ffast-math -mfp-mode=round-nearest -ffp-contract=fast"

    create_summary $master_sha
else
    echo No new commits for: $orig_branch
fi


openprs=$(curl --silent https://api.github.com/repos/parallella/pal/pulls?state=open | grep '"number"' | cut -f2 -d":" | tr -d " ," | sort -g)
#openprs=$(git branch -r --no-merged master | tr -d " " | grep "^origin/pr" | sort -g)

for n in $openprs; do
    echo Building open pull request: $n
    if ! need_build pr-$n $(git rev-parse origin/pr/$n); then
        echo No new commits for: pr-$n
        continue
    fi
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

if [ "x${created_pal_reports}" = "xyes" ]; then
    cd $PAL_REPORTS
    git push
    cd $top_srcdir
    rm -rf $PAL_REPORTS
fi
