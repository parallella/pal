#!/bin/bash
set -e

PAL_DB=${PAL_DB:-pal.db}

usage() {
    echo Usage: $0 [HEAD] >&2
    echo >&2
    echo Create HTML size summary >&2
    echo Example: $0 >&2
    exit 1
}

[ $# = 0 -o $# = 1 ] || usage
if [ $# = 1 ]; then
    head=$1
else
    head=$(git rev-parse HEAD)
fi

if ! which sqlite3 >/dev/null; then
    echo This tool needs sqlite3 >&2
    exit 1
fi

if ! which gawk >/dev/null; then
    echo This tool needs gawk >&2
    exit 1
fi

if ! [ -e ${PAL_DB} ]; then
    echo Database \'${PAL_DB}\' not found >&2
    exit 1
fi

files_qry="SELECT DISTINCT file FROM report WHERE commit_sha=\"${head}\" ORDER BY file ASC;"
files=$(echo "$files_qry" | sqlite3 ${PAL_DB});

cat << EOF
<html>
<head>
<title>PAL code summary</title>
<style>
.increase {
    background-color: red;
}
.decrease {
    background-color: green;
}

table {
    border-collapse: collapse;
    border: 1px solid #222;
    box-shadow: 2px 2px 10px #444;
}
th {
    border-bottom: 1px solid #222;
}
tr.newsymbol {
    border-top: 1px solid #222;
}
td, th {
    border-left: 1px solid #222;
    border-right: 1px solid #222;
    padding-left: 1em;
    padding-right: 1em;
}

th.side {
    border: 1px solid #222;
    background-color: #222;
    color: #eee;
}

.bottom {
    border: 1px solid #222;
}

.rotate {
    -ms-transform: rotate(90deg); /* IE 9 */
    -webkit-transform: rotate(90deg); /* Chrome, Safari, Opera */
    transform: rotate(90deg);
    padding-top: 50%;
    padding-bottom: 50%;
    margin-left: -90%;
}

.commit, .commit a, .symbol {
    font-family: "Courier New", Courier, monospace;
}
.commit, .commit a {
    min-width: 64ch;
}
.symbol {
    min-width: 24ch;
}
.number {
    text-align: right;
    min-width: 6ch;
}
</style>
</head>
<body>
<h1>PAL code size summary</h1>
<p>Latest commit: ${head}</p>

<table>
<tr><th rowspan="2">&nbsp;</th><th>x86_64</th><th>ARM</th><th>Epiphany</th><th class="side"><div class="rotate">Platform</div></th></tr>
<tr><th>(default)</th><th>(default)</th><th>(default)</th><th class="side"><div class="rotate">CFLAGS</div></th></tr>
<tr><th>File</th><th colspan="3">Size</th><th class="side" rowspan="1000">&nbsp;</th></tr>

EOF


size_qry() {
    c=$1
    f=$2
    p=$3
    cflags=$4
    echo "SELECT SUM(size) FROM report WHERE commit_sha='${c}' AND file='${f}' AND platform='${p}' AND cflags='${cflags}';"
}

for f in $files; do
    f_src=$(echo $f | sed 's/\.o$/\.c/g')
    x86_64_size=$((echo .mode csv && size_qry $head $f x86_64 "") | sqlite3 ${PAL_DB})
    arm_size=$((echo .mode csv && size_qry $head $f arm-linux-gnueabihf "") | sqlite3 ${PAL_DB})
    epiphany_size=$((echo .mode csv && size_qry $head $f epiphany-elf "") | sqlite3 ${PAL_DB})
    echo "<tr>"
    echo "<td><a href=\"https://github.com/parallella/pal/tree/master/${f_src}\">${f_src}</a></td>"
    echo "<td class=\"number\">${x86_64_size}</td>"
    echo "<td class=\"number\">${arm_size}</td>"
    echo "<td class=\"number\">${epiphany_size}</td>"
    echo "</tr>"
done

cat << EOF
<tr class="bottom"><td colspan="4">&nbsp;</td></tr>
</table>
</body>
</html>
EOF

