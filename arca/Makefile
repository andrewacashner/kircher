# Makefile for ARCA
# Andrew A. Cashner, 2019/02/14
#
SHELL 	= /bin/sh
CC    	= gcc
ADD_CFLAGS = 
CFLAGS 	= -Wall -ansi -pedantic -g $(ADD_CFLAGS)
IPATH 	= -Iinclude

exec_name = arca

src_dir  = src
bin_dir  = bin
build_dir = build
obj_dir  = obj
head_dir = include

dir_setup = $(bin_dir) $(obj_dir) $(obj_dir)/syntagma1

src   = $(wildcard $(src_dir)/*.c $(src_dir)/*/*.c)
head  = $(wildcard $(head_dir)/*.h $(head_dir)/*/*.h)
obj   = $(patsubst $(src_dir)/%.c,$(obj_dir)/%.o,$(src))

exec  = $(bin_dir)/$(exec_name)

.PHONY : all clean

all : $(exec)

$(exec) : $(obj) 
	$(CC) $(CFLAGS) -o $@ $(obj)

$(obj): $(src) $(head)

$(obj_dir)/%.o : $(src_dir)/%.c $(head_dir)/%.h | $(dir_setup)
	$(CC) $(CFLAGS) $(IPATH) -o $@ -c $<

$(dir_setup) :
	-mkdir -p $@

clean : 
	-rm -f $(bin_dir)/* $(build_dir)/* $(obj_dir)/*.o $(obj_dir)/*/*.o
