#!/usr/bin/env bash
#
# Run the arca a specified number of times for a given input and tone

if [ "$#" -ne 6 ]
then
    echo "Usage: arca-nochance-intone-rerun.sh TONE[1-12] INFILE.xml OUTFILE-basename [--ficta|--noficta] [--simple|--florid] REPETITIONS[1-]"
    exit
fi

tone="$1"
infile="$2"
out_stem="$3"
ficta_switch="$4"
style_switch="$5"
n="$6"

out_base="$out_stem-tone$(printf '%02d' $tone)"

logfile="$out_base.log"

if [ -f "$logfile" ]
then
    rm "$logfile"
fi

success=true
for i in $(seq 1 $n)
do 
    next_outfile="$out_base-$(printf '%04d' $i)"
    ./arca-nochance-intone.sh "$tone" "$infile" "$next_outfile" "$ficta_switch" "$style_switch"
    if [ "$?" -eq 0 ]
    then 
        echo "$(sha1sum $next_outfile.mei)" >> "$logfile"
    else
        success=false
    fi
done

if "$success"
then
    echo "SHA checksums written to log file $logfile"
else
    echo "No Arca output"
fi

