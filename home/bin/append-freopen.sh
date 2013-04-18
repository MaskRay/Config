#!/bin/bash
if [ -z "$1" ]; then
    echo "Usage: $0 prog" 1>&2
    exit
fi
tmpfile=`mktemp`
echo "    freopen(\"$1.in\", \"r\", stdin);" > $tmpfile
echo "    freopen(\"$1.out\", \"w\", stdout);" >> $tmpfile
sed "/^int main()$/{N;r $tmpfile
}"
rm $tmpfile
