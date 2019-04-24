SHELL	= /usr/bin/env sh

scm	= $(wilcard kircher/*.scm)
input	= $(wildcard input/*.xml)
mei	= $(patsubst input/%.xml,build/%.mei,$(input))
svg	= $(mei:%.mei=%_001.svg)
pdf	= $(addprefix build/,$(notdir $(input:%.xml=%.pdf)))

.PHONY : all pdf view clean

all : $(svg)

pdf : $(pdf)

$(svg) : $(mei)

$(mei) : $(input) $(scm)


build/%_001.svg : build/%.mei
	verovio -a --scale 40 --mensural-to-measure $<

build/%.mei : input/%.xml
	sh ./kircher/main.scm $< $@

$(pdf) : $(svg)
	for i in build/*.svg; do rsvg-convert -f pdf -o "$$i{%.svg}.pdf" "$$i"; done
	pdf-stapler cat build/*.pdf $(pdf)

view : $(svg)
	chromium-browser build/*.svg &> /dev/null &

clean :
	rm build/*


