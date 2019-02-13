/* scribo.h */
#ifndef SCRIBO_H
#define SCRIBO_H

#include <stdio.h>
#include "lectio.h"
#include "arca.h"

#define LY_VERSION "2.19"

void list_print_text(FILE *outfile, node_ptr ls);
void print_lyrics(FILE *outfile, node_ptr ls);
void print_version(FILE *outfile, char *v_num);
char *voice_part(char *str, int voice, int meter);
void compose(FILE *outfile, node_ptr ls, 
        syntagma_ptr syntagma, int mode, int meter);

#endif
