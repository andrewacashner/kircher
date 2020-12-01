#!/usr/bin/env sh
set -e
xml="$1"
ly="test/$(basename $xml .xml).ly"
pdf="$(basename $ly .ly).pdf"

stack run "$xml" "$ly"
cd $(dirname $ly)
lilypond --silent -I "$HOME"/lib/ly "$(basename $ly)"
mupdf "$pdf"
