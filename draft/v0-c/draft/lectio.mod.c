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

/* CONSTANTS */
#define MAX_NODE_CHAR 100

enum { SHORT, LONG, MAX_SYL_TYPE } syl_type_code;
int syl_type_num[] = { SHORT, LONG, MAX_SYL_TYPE };
char *syl_type_char = "SL";

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
void list_print(node_ptr ls);
node_ptr new_lyric_node(char *str);
node_ptr text_list(node_ptr ls, FILE *infile);

/* FUNCTIONS */ 
int length_val(char c) {
    int i;
    for (i = 0; i < MAX_SYL_TYPE; ++i) {
        if (syl_type_char[i] == c) {
            break;
        }
    }
    return(syl_type_num[i]);
}

node_ptr node_create(void) {
    node_ptr new = malloc(sizeof(node));
    return(new);
}

node_ptr node_set(node_ptr new, int syl, int len, char *str) {
    new->next = NULL;
    new->syllables = syl;
    new->penult_len = len;
    strcpy(new->text, str);

    return(new);
}

node_ptr last(node_ptr ls) {
    if (ls->next != NULL) {
        ls = last(ls->next);
    }
    return(ls);
}

node_ptr list_append(node_ptr ls, node_ptr new) {
    node_ptr head = ls;
    assert(new != NULL);
    if (ls == NULL) {
        head = new;
    } else {
        last(ls)->next = new;
    }
    return(head);
}

void list_free(node_ptr ls) {
    if (ls == NULL) {
        free(ls);
    } else {
        list_free(ls->next);
    }
    return;
}

void list_print_text(node_ptr ls) {
    if (ls != NULL) {
        printf("%s\n", ls->text);
        list_print(ls->next);
    }
    return;
}


node_ptr new_lyric_node(char *str) {
    int syl_count, syl_type;
    char syl_type_c;
    node_ptr new = node_create();
  
    syl_count = atoi(str);
    syl_type_c = str[2];
    syl_type = length_val(syl_type_c);
    new = node_set(new, syl_count, syl_type, &str[4]);
    return(new);
}

node_ptr text_list(node_ptr ls, FILE *infile) { 
    char line[MAX_CHAR];

    assert(infile != NULL);

    while (fgets(line, sizeof(line), infile) != NULL) {
        switch (line[0]) {
            case '%':
                /* Comment line, ignore */
                continue;
            case '\n':
                /* Blank line, ignore */
                continue;
            default:
                line[strlen(line) - 1] = '\0';
                ls = list_append(ls, new_lyric_node(line));
                break;
        }
    }
    return(ls);
}

    /* TODO don't forget: list_free(ls); */








