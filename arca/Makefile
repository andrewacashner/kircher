# Makefile for ARCA
# Andrew A. Cashner, 2019/02/14
#
SHELL 	= /bin/sh
CC    	= gcc
CFLAGS 	= -Wall -ansi -pedantic
IPATH 	= -Iinclude

exec_name = arca

src_dir  = src
bin_dir  = bin
obj_dir  = obj
head_dir = include

src   = $(wildcard $(src_dir)/*.c $(src_dir)/*/*.c)
head  = $(wildcard $(head_dir)/*.h $(head_dir)/*/*.h)
obj   = $(patsubst $(src_dir)/%.c,$(obj_dir)/%.o,$(src))

exec  = $(bin_dir)/$(exec_name)

.PHONY : all clean

all : $(exec)

$(exec) : $(obj) 
	$(CC) $(CFLAGS) -o $@ $(obj)

$(obj_dir)/%.o : $(src_dir)/%.c $(head_dir)/%.h
	$(CC) $(CFLAGS) $(IPATH) -o $@ -c $<

clean : 
	-rm -f $(bin_dir)/* $(obj_dir)/*.o $(obj_dir)/*/*.o
