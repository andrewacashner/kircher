SHELL	= /usr/bin/sh

input	= $(wildcard input/*.xml)
mei	= $(patsubst input/%.xml,build/%.mei,$(input))
svg	= $(mei:%.mei=%_001.svg)

.PHONY : all view clean

all : $(svg)

$(svg) : $(mei)

$(mei) : $(input)

build/%_001.svg : build/%.mei
	verovio -a --page-width 60000 $<

build/%.mei : input/%.xml
	./main.scm $< $@

view : $(svg)
	firefox build/*.svg &

clean :
	rm build/*


