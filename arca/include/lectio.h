/* lectio.h
 * Andrew A. Cashner
 * 2019/02/12
 *
 * Module for reading text input for Kircher's Arca
 *
 * TODO
 * Eventually: Read in unadorned Latin text, parse it into syllables and
 * phrases, determine syllable counts and syllable lengths. Perhaps even
 * determine the style, meter, etc.
 *
 * First step: Read in a file in simple format, e.g.:
 * 4 L\tAl -- le -- lu -- ia.\n
 * Where 4 is the number of syllables in the line, 
 * L means the penultimate syllable is long,
 * and the newline indicates the end of this segment.
 * Given a choice of syntagma 1 and this input, the arca program would then
 * select pinax 1, col 3.
 */

/* - Open input file
 * - For each line:
 * - Skip comment lines starting with %
 * - Skip whitespace lines
 * - Read first char (%d) as syllable count (in new linked-list node)
 * - Read third (next non-whitespace) char (%c) as L (long) or S (short) to set
 *   length (in linked-list node)
 * - Skip following whitespace.
 * - Read up to newline and store string in a linked-list node.
 * - Repeat until end of file.
 */
#ifndef LECTIO_H
#define LECTIO_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

/* CONSTANTS */
#define MAX_CHAR 256*100
#define MAX_NODE_CHAR 100

/* DATA STRUCTURES */
typedef struct textlist *textlist_ptr;
typedef struct textlist {
    textlist_ptr next;
    int syllables;
    int penult_len;
    char text[MAX_NODE_CHAR];
} textlist;

/* FUNCTION PROTOTYPES */
int length_val(char c);
textlist_ptr textlist_create(void);
textlist_ptr textlist_set(textlist_ptr new, int syl, int len, char *str);
textlist_ptr last(textlist_ptr ls);
textlist_ptr list_append(textlist_ptr ls, textlist_ptr new);
void list_free(textlist_ptr ls);
textlist_ptr new_lyric_textlist(char *str);
textlist_ptr text_list(textlist_ptr ls, FILE *infile);

#endif
