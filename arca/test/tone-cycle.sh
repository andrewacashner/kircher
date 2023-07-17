#!/usr/bin/env bash

# Tone cycle
# Andrew Cashner, 2023/07/14
#
# Given an arca XML input file with "{tone}" in place of tone number (e.g.,
# @tone='{tone}') and "{style}" for style attribute value, run the arca on
# this file in both styles, in every permissible tone; save the results in a
# single PDF.
#
# Use {toneName} for the Latin genitive tone name in the title (e.g., "Quinti 
# Toni" for tone 5). Use {styleName} for the Latin name of the style type
# (Simplex vel Floridus).

if [ "$#" -ne 3 ]
then
    echo "Usage: tone-cycle.sh [--ficta|--noficta] [--simple|--florid] INFILE.xml"
    exit
fi

infile="$3"

if [ ! -f "$infile" ]
then
    echo "File $infile not found" >&2
    exit
fi

base=$(basename -s .xml "$infile")

ficta_switch="$1"
case "$ficta_switch" in
    ("--ficta")   ficta=true ;;
    ("--noficta") ficta=false ;;
    (*)           "Incorrect ficta specification" >&2 ; exit ;;
esac

style_switch="$2"
case "$style_switch" in 
    ("--simple")  styleNum="0" ;;
    ("--florid")  styleNum="1" ;;
    (*)           "Incorrect style specification" >&2 ; exit ;;
esac
style=(Simple Florid)
styleName=(Simplex Floridus)
thisStyle="${style[$styleNum]}"
thisStyleName="${styleName[$styleNum]}"

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


for n in {1..12} 
do
    # Two-digit numbers for sorting
    printf -v N "%02d" "$n" 
    baseStyle="output/$base-$styleNum-$thisStyle"
    if [ "$ficta" == "false" ] 
    then 
        baseStyle="$baseStyle-noficta"
    fi
    baseN="$baseStyle-tone$N"


    ordinal="${numeros[$n - 1]} Toni"
    sed -e "s/{tone}/Tone$n/; s/{toneName}/$ordinal/;
    s/{style}/$thisStyle/; s/{styleName}/$thisStyleName/" \
        "$infile" > "$baseN.xml"

    if "$ficta"
    then
        stack run arca-nochance "$baseN.xml" "$baseN.mei" perms.hs \
            2> "$baseN.log"
    else
        stack run arca-nochance "$baseN.xml" "$baseN.mei" perms.hs noFicta
    fi

    if [ "$?" -ne 0 ]
    then
        echo "arca exited with error"
        rm "$baseN.mei"
        rm "$baseN.log"
    else
        verovio -r /opt/local/share/verovio -a "$baseN.mei"
        rm "$baseN.mei"

        for svgFile in "$baseN"*.svg
        do
            echo "Found svg file $svgFile"
            inkscape "$svgFile" -o "${svgFile%.svg}.pdf"
            rm "$svgFile"
        done

        if [ -f "$baseN"_002.pdf ]
        then
            pdfunite "$baseN"_*.pdf "$baseN.pdf"
            rm "$baseN"_*.pdf
        else
            mv "$baseN"_001.pdf "$baseN.pdf"
        fi
        echo "Arca PDF written to $baseN.pdf"
        rm "$baseN.xml"
    fi
done

pdf_out="$baseStyle.pdf"

pdfunite "$baseStyle-tone"*.pdf "$pdf_out" \
    && echo "PDF output written to $pdf_out"

rm "$baseStyle-tone"*.pdf

