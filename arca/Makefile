CFLAGS = -Wall -ansi -pedantic

in = $(wildcard *.c)
out = $(addsuffix x,$(in))

.PHONY : all

all : $(out)

%.cx : %.c
	gcc $(CFLAGS) -o $@ $<

