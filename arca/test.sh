#! /usr/bin/env sh

for m in {1..12}
do
    ./bin/arca -s 1 -m "$m" -t 2 text/ps-150.txt "$m".ly
    lilypond "$m"
    mupdf "$m".pdf
done



