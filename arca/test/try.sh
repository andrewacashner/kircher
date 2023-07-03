#!/usr/bin/env sh
set -e

in="$1"
out="$2"

arca "$in" "$out"
verovio -r /opt/local/share/verovio --all-pages "$out"
inkview "${out%.mei}"*.svg &
verovio -r /opt/local/share/verovio -t midi "$out"
env PULSE_LATENCY_MSEC=80 timidity "${out%.mei}.mid"

