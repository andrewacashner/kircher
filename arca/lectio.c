/* lectio.c 
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* CONSTANTS */
#define MAX_CHAR 256*100
#define MAX_NODE_CHAR 100

enum { SHORT = 'S', LONG = 'L', OTHER } length_codes;
int length_num[] = { SHORT, LONG, OTHER };

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
node_ptr node_create(int syl, int len, char *str);
node_ptr node_append(node_ptr ls, node_ptr new);
node_ptr list_create(void);
void list_print(node_ptr ls);
node_ptr new_lyric_node(char *str);


/* MAIN */
int main(int argc, char *argv[]) {
    FILE *infile;
    char *infilename;
    char line[MAX_CHAR];
    node_ptr ls = list_create();

    if (argc != 2) {
        exit(EXIT_FAILURE);
    }

    infilename = argv[1];
    infile = fopen(infilename, "r");
    if (infile == NULL) {
        exit(EXIT_FAILURE);
    }

    while (fgets(line, sizeof(line), infile) != NULL) {
        switch (line[0]) {
            case '%':
                /* Comment line, ignore */
                continue;
            case '\n':
                /* Blank line, ignore */
                continue;
            default:
                ls = node_append(ls, new_lyric_node(line));
        }
    }

    list_print(ls);

    fclose(infile);
    return(0);
}


/* FUNCTIONS */ 
int length_val(char c) {
    assert(c == 'S' || c == 'L');
    return(length_num[(int)c]);
}

node_ptr node_create(int syl, int len, char *str) {
    node_ptr new = malloc(sizeof(node_ptr));
    new->next = NULL;
    new->syllables = syl;
    new->penult_len = len;
    strcpy(new->text, str);
    return(new);
}

node_ptr node_append(node_ptr ls, node_ptr new) {
    node_ptr head = ls;
    assert(new != NULL);
    if (ls == NULL) {
        head = new;
    } else {
        while (ls->next != NULL) {
            ls = ls->next;
        }
        ls->next = new;
    }
    return(head);
}

node_ptr list_create(void) {
    node_ptr new = malloc(sizeof(node_ptr));
    return(new);
}

void list_print(node_ptr ls) {
    if (ls != NULL) {
        printf("%s\n", ls->text);
        list_print(ls->next);
    }
    return;
}


node_ptr new_lyric_node(char *str) {
    int syl, len;
    char len_char;
    char *c_ptr = NULL;
    node_ptr new = NULL;
   
    sscanf(str, "%d %c", &syl, &len_char);
   
    len = length_val(len_char);

    c_ptr = strchr(str, '\t');
    ++c_ptr;

    new = node_create(syl, len, c_ptr);
    return(new);
}








