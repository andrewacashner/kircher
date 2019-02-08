CFLAGS = -Wall -ansi -pedantic

build = test
dirs = $(build)
in = $(wildcard *.c)
exec = $(addsuffix x,$(in))
pdf = $(build)/test.pdf

.PHONY : all pdf view clean

all : $(exec)

pdf : $(pdf) 

$(dirs) :
	-mkdir -p $(dirs)

%.cx : %.c
	gcc $(CFLAGS) -o $@ $<

$(build)/%.arca : $(exec) | $(dirs)
	./arca.cx > $@

$(build)/%.ly : $(build)/%.arca  
	./arca2ly.cx $< $@

$(build)/%.pdf : $(build)/%.ly
	lilypond -o $(basename $@) $<

view : $(pdf) 
	mupdf $<

clean : 
	-rm -rf $(build) $(exec)
