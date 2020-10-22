#!/usr/bin/env sh

set -e

text="Lau-\`da-te \`Do-mi-num \`om-nis \`ter-rae. Al-le-\`lu-ia. A-\`men."

echo "$text" | stack run > test/test.ly
cd test
lilypond -I "$HOME"/lib/ly test
mupdf test.pdf
