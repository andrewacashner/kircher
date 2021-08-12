#!/usr/bin/env sh

in="$1"
out="$2"

arca "$in" "$out"
verovio --all-pages "$out"
verovio -t midi "$out"
inkview "${out%.mei}"*.svg &
timidity "${out%.mei}.mid"

