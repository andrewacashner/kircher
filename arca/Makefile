CC = gcc
CFLAGS = -Wall -ansi -pedantic

exec = arca.cx arca2ly.cx
dirs = build
pdf = build/arca.pdf

.PHONY : all pdf view clean

all : $(exec)

pdf : $(pdf) 

$(dirs) :
	-mkdir -p $(dirs)

%.o : %.c %.h

arca.cx : arca.o lectio.o
	$(CC) $(CFLAGS) -o $@ arca.o lectio.o

arca2ly.cx : arca2ly.c 
	$(CC) $(CFLAGS) -o $@ $<

build/arca.txt : arca.cx | $(dirs)
	./arca.cx texts/ps-150.txt > $@

build/%.ly : build/%.txt  arca2ly.cx
	./arca2ly.cx $< $@

build/%.pdf : build/%.ly
	lilypond -o $(basename $@) $<

view : $(pdf) 
	mupdf $<

clean : 
	-rm -rf $(dirs)
