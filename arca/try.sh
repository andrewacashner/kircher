#!/usr/bin/env sh
set -e

stack run input/Ps-150.xml test/test.ly
cd test
lilypond -I "$HOME"/lib/ly test
mupdf test.pdf
