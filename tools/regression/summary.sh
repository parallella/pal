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

# CFLAGS have to match perfectly with mkallbranches.sh
# ??? TODO: Use separate file for CFLAGS
x86_64_Os_flags='-Os -ffast-math'
x86_64_O2_flags='-O2 -ffast-math'
arm_Os_flags='-Os -ffast-math'
arm_O2_flags='-O2 -ffast-math'
epiphany_Os_flags='-Os -ffast-math -mfp-mode=round-nearest -ffp-contract=fast'
epiphany_O2_flags='-O2 -ffast-math -mfp-mode=round-nearest -ffp-contract=fast'

# Fix word wrapping
fixwrap() {
    echo $1 | sed -e 's,^,<nobr>,g;s, [ ]*\([^ ]\),</nobr> <nobr>\1,g;s,$,</nobr>,g'
}

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
    width: 5em; /* hack? but it works */
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

th.cflags {
    font-size: x-small;
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
<tr><th rowspan="2">&nbsp;</th><th colspan="3">x86_64</th><th colspan="3">ARM</th><th colspan="3">Epiphany</th><th class="side"><div class="rotate">Platform</div></th></tr>
<tr>
<th class="cflags">(default)</th><th class="cflags">$(fixwrap "${x86_64_Os_flags}")</th><th class="cflags">$(fixwrap "${x86_64_O2_flags}")</th>
<th class="cflags">(default)</th><th class="cflags">$(fixwrap "${arm_Os_flags}")</th><th class="cflags">$(fixwrap "${arm_O2_flags}")</th>
<th class="cflags">(default)</th><th class="cflags">$(fixwrap "${epiphany_Os_flags}")</th><th class="cflags">$(fixwrap "${epiphany_O2_flags}")</th>
<th class="side"><div class="rotate">CFLAGS</div></th></tr>
<tr><th>File</th><th colspan="9">Size</th><th class="side" rowspan="1000">&nbsp;</th></tr>

EOF


size_qry() {
    f=$1
    p=$2
    cflags=$3
    (
        echo ".mode csv"
        echo "SELECT SUM(size) FROM report WHERE commit_sha='${head}' AND file='${f}' AND platform='${p}' AND cflags='${cflags}';"
    ) | sqlite3 ${PAL_DB}
}

for f in $files; do
    f_src=$(echo $f | sed 's/\.o$/\.c/g')
    echo "<tr>"
    echo "<td><a href=\"https://github.com/parallella/pal/tree/master/${f_src}\">${f_src}</a></td>"
    echo "<td class=\"number\">$(size_qry $f x86_64 '')</td>"
    echo "<td class=\"number\">$(size_qry $f x86_64 "${x86_64_Os_flags}")</td>"
    echo "<td class=\"number\">$(size_qry $f x86_64 "${x86_64_O2_flags}")</td>"
    echo "<td class=\"number\">$(size_qry $f arm-linux-gnueabihf '')</td>"
    echo "<td class=\"number\">$(size_qry $f arm-linux-gnueabihf "${arm_Os_flags}")</td>"
    echo "<td class=\"number\">$(size_qry $f arm-linux-gnueabihf "${arm_O2_flags}")</td>"
    echo "<td class=\"number\">$(size_qry $f epiphany-elf '')</td>"
    echo "<td class=\"number\">$(size_qry $f epiphany-elf "${epiphany_Os_flags}")</td>"
    echo "<td class=\"number\">$(size_qry $f epiphany-elf "${epiphany_O2_flags}")</td>"
    echo "</tr>"
done

cat << EOF
<tr class="bottom"><td colspan="10">&nbsp;</td></tr>
</table>
</body>
</html>
EOF

