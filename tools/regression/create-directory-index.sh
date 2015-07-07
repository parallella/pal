#!/bin/bash

if ! which gawk >/dev/null; then
    echo This tool needs gawk >&2
    exit 1
fi

if [ "x${PAL_REPORTS}" = "x" ]; then
    echo $0: PAL_REPORTS not set >&2
    exit 1
fi

cd $PAL_REPORTS

for d in $(find . -type d -not -wholename './.git*'); do
    (
        echo -e "<html>\n<body>\n<h1>Index of $d</h1>\n<hr/>\n<pre>"
        ls -1pa "${d}" | grep -v "^\./$" | grep -Ev "^index\.html$|^\.git/$" | gawk '{ printf "<a href=\"%s\">%s</a>\n",$1,$1 }'
        echo -e "</pre>\n</body>\n</html>"
    ) > $d/index.html
    git add $d/index.html
done

git commit -m "Generated directory index" || echo "Directory index unchanged"
