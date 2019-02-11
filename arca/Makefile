CFLAGS = -Wall -ansi -pedantic

bin = bin
build = build
dirs = $(build) $(bin)
in = $(wildcard *.c)
exec = $(in:%.c=$(bin)/%.cx)
pdf = $(build)/arca.pdf

.PHONY : all pdf view clean

all : $(exec)

pdf : $(pdf) 

$(dirs) :
	-mkdir -p $(dirs)

$(bin)/%.cx : %.c | $(dirs)
	gcc $(CFLAGS) -o $@ $<

$(build)/%.txt : $(bin)/arca.cx
	./$(bin)/arca.cx > $@

$(build)/%.ly : $(build)/%.txt  $(bin)/arca2ly.cx
	./$(bin)/arca2ly.cx $< $@

$(build)/%.pdf : $(build)/%.ly
	lilypond -o $(basename $@) $<

view : $(pdf) 
	mupdf $<

clean : 
	-rm -rf $(dirs)
