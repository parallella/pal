#!/bin/bash

usage() {
    echo Usage: $0 DIRECTORY >/dev/stderr
    echo
    echo Reports stack size per file and function.
    exit 1
}

[ $# == 1 ] || usage

dir=$1
files=$(find $dir -name "*.su" | grep -v "\.libs" | sort)
echo "Size      File/Symbol                                       Type"
echo "==================================================================="

for f in $files; do
    (echo $f && cat $f) | gawk '
    {
        if (NR==1) {
            file=$1;
            next;
        }
        sum += $2;
        if (txt == "")
            txt=sprintf("%d,%s,%s",$2,$1,$3);
        else
            txt=sprintf("%s,%d,%s,%s",txt,$2,$1,$3);
    }; END { printf("%d,%s,%s\n", sum, file, txt); } '
done | sort -r -t, -g | gawk -F, '
    {
        printf("%6d    %.32s\n", $1, $2);
        for (i = 3; i <= NF; i = i+3) {
            j=i+1; k=i+2;
            printf("%6d    %-50.50s%-16.16s\n", $i, $j, $k);
        }
        printf("\n");
    } '
