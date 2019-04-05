SHELL	= /usr/bin/sh

scm	= $(wilcard kircher/*.scm)
input	= $(wildcard input/*.xml)
mei	= $(patsubst input/%.xml,build/%.mei,$(input))
svg	= $(mei:%.mei=%_001.svg)

.PHONY : all view clean

all : $(svg)

$(svg) : $(mei)

$(mei) : $(input) $(scm)

build/%_001.svg : build/%.mei
	verovio -a --scale 40 $<

build/%.mei : input/%.xml
	sh ./kircher/main.scm $< $@

view : $(svg)
	chromium-browser build/*.svg &> /dev/null &

clean :
	rm build/*


