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
typedef struct node *node_ptr;
typedef struct node {
    node_ptr next;
    int syllables;
    int penult_len;
    char text[MAX_NODE_CHAR];
} node;

/* FUNCTION PROTOTYPES */
int length_val(char c);
node_ptr node_create(void);
node_ptr node_set(node_ptr new, int syl, int len, char *str);
node_ptr last(node_ptr ls);
node_ptr list_append(node_ptr ls, node_ptr new);
void list_free(node_ptr ls);
void list_print_text(node_ptr ls);
node_ptr new_lyric_node(char *str);
node_ptr text_list(node_ptr ls, FILE *infile);

#endif
