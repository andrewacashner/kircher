SHELL	= /usr/bin/env sh

scm	= $(wilcard kircher/*.scm)
input	= $(wildcard input/*.xml)
ly	= $(addprefix build/,$(notdir $(input:%.xml=%.ly)))
pdf 	= $(ly:%.ly=%.pdf)

.PHONY : all view clean

all : $(pdf)

$(pdf) : $(ly)

build/%.ly : input/%.xml
	sh ./kircher/main.scm $< $@

build/%.pdf : build/%.ly
	lilypond -I $(HOME)/ly -o build/$* $<

view : $(pdf)
	xpdf $< &

clean :
	rm build/*


