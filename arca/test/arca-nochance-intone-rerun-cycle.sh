#!/usr/bin/env bash
#
# Run a given number of repetitions of the Arca, in every tone

if [ "$#" -ne 5 ]
then
    echo "Usage: arca-nochance-intone.sh INFILE.xml OUTFILE.mei [--ficta|--noficta] [--simple|--florid] REPETITIONS[1-]"
    exit
fi


infile="$1"
outfile="$2"
ficta_switch="$3"
style_switch="$4"
repetitions="$5"

for tone in {1..12}
do 
    ./arca-nochance-intone-rerun.sh "$tone" "$infile" "$outfile" "$ficta_switch" "$style_switch" "$repetitions"
done

logfile="$2-alltones.log"

if [ -f "$logfile" ]
then
    rm "$logfile"
fi

for file in "$2"-tone*.log
do
    cat "$file" >> "$logfile"
    echo >> "$logfile"
done

echo "Summary SHA sum comparison written to $logfile"
