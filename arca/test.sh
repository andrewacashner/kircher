#! /usr/bin/env sh
set -e

mode="$1"
tempus="$2"
target="build/test"

./bin/arca -s 1 -m "$mode" -t "$tempus" text/ps-150.txt "$target".ly
lilypond -o "$target" "$target"
mupdf "$target".pdf

