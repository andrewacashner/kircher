#!/usr/bin/env bash

outfile="output/total-corrections.log"
echo "ARCA MUSARITHMICA FICTA CORRECTIONS" > "$outfile"

for file in output/*.log
do 
    wc -l "$file" | \
        sed -e 's/\([0-9].*\) \(.*\)-\(tone[0-9]*\).log/\1\t\3\t\2/' >> "$outfile"
done
