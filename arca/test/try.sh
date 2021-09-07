#!/usr/bin/env sh

in="$1"
out="$2"

arca "$in" "$out"
verovio --all-pages "$out"
inkview "${out%.mei}"*.svg &
verovio -t midi "$out"
env PULSE_LATENCY_MSEC=80 timidity "${out%.mei}.mid"

