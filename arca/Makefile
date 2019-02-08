CFLAGS = -Wall -ansi -pedantic

build = test
bin = bin
dirs = $(build) $(bin)
in = $(wildcard *.c)
exec = $(in:%.c=$(bin)/%.cx)
pdf = $(build)/test.pdf

.PHONY : all pdf view clean

all : $(exec)

pdf : $(pdf) 

$(dirs) :
	-mkdir -p $(dirs)

$(bin)/%.cx : %.c | $(dirs)
	gcc $(CFLAGS) -o $@ $<

$(build)/%.arca : $(bin)/arca.cx
	./$(bin)/arca.cx > $@

$(build)/%.ly : $(build)/%.arca  $(bin)/arca2ly.cx
	./$(bin)/arca2ly.cx $< $@

$(build)/%.pdf : $(build)/%.ly
	lilypond -o $(basename $@) $<

view : $(pdf) 
	mupdf $<

clean : 
	-rm -rf $(dirs)
