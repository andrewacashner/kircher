#!/usr/bin/env sh

set -e

stack run > test/test.ly
cd test
lilypond test
mupdf test.pdf
