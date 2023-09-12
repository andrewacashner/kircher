#!/usr/bin/env bash
#
# Run the arca with preselected permutations (no chance element), in a given
# tone
#
# Defaults: 
#   - Simple style
#
# Andrew Cashner, 2023/09/12

if [ "$#" -ne 5 ]
then
    echo "Usage: arca-nochance-intone.sh TONE[1-12] INFILE.xml OUTFILE[basename] [--ficta|--noficta] [--simple|--florid]"
    exit
fi

n="$1"
infile="$2"
out_base="$3"
ficta_switch="$4"
style_switch="$5"

mei_out="$3.mei"

case "$ficta_switch" in
    ("--ficta")   ficta="ficta" ;;
    ("--noficta") ficta="noFicta" ;;
    (*)           "Incorrect ficta specification" >&2 ; exit ;;
esac

case "$style_switch" in
    ("--simple")    style="Simple" ;;
    ("--florid")    style="Florid" ;;
    (*)             "Incorrect style specification" >&2 ; exit ;;
esac

if [ "$style" == "Simple" ]
then
    style_name="Simplex"
else
    style_name="Floridus"
fi


infile_tone="${infile%.xml}-tone$n.xml"

numeros=( 
Primi
Secundi
Tertii
Quarti
Quinti
Sexti
Septimi
Octavi
Noni
Decimi
Endecimi
Duodecimi
)

ordinal="${numeros[$n - 1]} Toni"

sed -e "s/{tone}/Tone$n/; s/{toneName}/$ordinal/; s/{style}/$style/; s/{styleName}/$style_name/" \
    "$infile" > "$infile_tone"
if [ "$?" -eq 0 ]; then echo "Created input file $infile_tone in tone $n"; fi

stack run arca-nochance "$infile_tone" "$mei_out" perms.hs "$ficta"
if [ "$?" -eq 0 ]
then 
    echo "Arca musarithmica generated MEI output $mei_out"
fi
rm "$infile_tone"

