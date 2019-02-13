SHELL = /bin/sh
CC    = gcc
CFLAGS = -Wall -ansi -pedantic
IPATH = -Iinclude

exec  = bin/arca 
src   = $(wildcard src/*.c src/*/*.c)
head  = $(wildcard include/*.h include/*/*.h)
obj   = $(patsubst src/%.c,obj/%.o,$(src))

.PHONY : all clean

all : $(exec)

$(exec) : $(obj) 
	$(CC) $(CFLAGS) -o $@ $(obj)

obj/%.o : src/%.c include/%.h
	$(CC) $(CFLAGS) $(IPATH) -o $@ -c $<

clean : 
	-rm $(exec) $(obj)
