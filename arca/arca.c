/* vim: set foldmethod=syntax : */

/* arca.c
 * Andrew A. Cashner
 * 2019/02/04
 * Tests toward a digital version of Kircher's Arca musarithmica
 */

/* TODO
 * account for rests
 * assert rhythm value series in range
 * check note entry from kircher is correct
 * add octaves to notes and/or voices/clefs
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>

/* CONSTANTS, LABELS */

#define MAX_STR 100

/*      MODES (TONI) */
#define MAX_SCALE 8
enum { 
    nA, nBf, nB, nC, nCs, 
    nD, nEf, nE, nF, nFs, 
    nG, nGs, 
    MAX_PITCH 
} note_nums;

char *note_names[] = {
    "a", "bes", "b", "c", "cis", 
    "d", "es", "e", "f", "fis", 
    "g", "gis", 
    "ERROR"
};

enum { 
    MODE1, MODE2, MODE3, MODE4, 
    MODE5, MODE6, MODE7, MODE8, 
    MODE9, MODE10, MODE11, MODE12,
    MAX_MODE
} mode_names;

int mode[][MAX_SCALE] = {
    /* I */ 
    { nD, nE, nF, nG, nA, nBf, nCs, nD },
    /* II (mollis) */
    { nG, nA, nB, nC, nD, nEf, nFs, nG },
    /* III */
    { nA, nB, nC, nD, nE, nF, nGs, nA },
    /* IV */
    { nA, nB, nCs, nD, nE, nF, nG, nA },
    /* V */
    { nB, nC, nD, nE, nF, nG, nA, nB },
    /* VI (mollis) */
    { nF, nG, nA, nB, nC, nD, nE, nF },
    /* VII */
    { nG, nA, nB, nC, nD, nE, nFs, nG },
    /* VIII */
    { nG, nA, nB, nC, nD, nE, nFs, nG },
    /* IX (mollis) */
    { nD, nE, nF, nG, nA, nB, nCs, nD },
    /* X */
    { nA, nB, nC, nD, nE, nF, nG, nA },
    /* XI (mollis) */
    { nC, nD, nE, nF, nG, nA, nB, nC },
    /* XII */
    { nF, nG, nA, nB, nC, nD, nE, nF }
};

/*          modes that each can be mixed with? */    
/*          names and descriptions of modes */

/*      RHYTHMS */
enum { DUPLE, TRIPLA, TRIPLA_MINOR } rperm_type;

enum { 
    XX,     /* no value, blank */
    BRP,    /* breve perfect */
    BR,     /* breve imperfect */
    SBP,    /* semibreve perfect or dotted */
    SB,     /* semibreve imperfect */
    MND,    /* minim dotted */
    MN,     /* minim */
    SMD,    /* semiminim dotted */
    SM,     /* semiminim */
    FS,     /* fusa */
    rBR,    /* rest breve */
    rSB,    /* rest semibreve */
    rMN,    /* rest minim */
    rSM,    /* rest semiminim */
    MAX_RHYTHM
} rhythmic_values;

char *rhythm_names[] = {
    "",
    "\\breve.",
    "\\breve",
    "1.",
    "1",
    "2.",
    "2",
    "4.",
    "4",
    "8",
    "r\\breve",
    "r1",
    "r2",
    "r4",
    "ERROR"
};

char *roman[] = {
    "0",
    "I", "II", "III", "IV", "V", 
    "VI", "VII", "VIII", "IX", 
    "X", "XI", "XII"
};

/*      ARRAY BOUNDS */
#define VPERM_Z 10
#define VPERM_Y 4
#define VPERM_X 10

#define RPERM_Z 3
#define RPERM_Y 10
#define RPERM_X 10

#define MAX_COL 5

/* DATA STRUCTURES */
typedef struct {
    int array[VPERM_Z][VPERM_Y][VPERM_X];
} vperm;
typedef vperm *vperm_ptr;

typedef struct {
    int array[RPERM_Z][RPERM_Y][RPERM_X];
} rperm;
typedef rperm *rperm_ptr;

typedef struct col {
    int syl;
    vperm_ptr vperm;
    rperm_ptr rperm;
} col;
typedef col *col_ptr;

typedef struct pinax {
    int id;
    char *label;
    char *desc;
    int max_col;
    col_ptr *column;
} pinax;
typedef pinax *pinax_ptr;

typedef struct arca {
    int max_pinax;
    pinax_ptr *pinax;
} arca;
typedef arca *arca_ptr;

/* DATA */

#include "data/pinax1.c"

pinax_ptr pinaces_all[] = { &p1 };
arca kircher = { 1, pinaces_all };

/* FUNCTION PROTOTYPES */

pinax_ptr get_pinax_ptr(arca_ptr a, int i);
col_ptr get_col_ptr(pinax_ptr p, int i);
int get_pitch_num(col_ptr c, int z, int y, int x);
char *get_note_name(int pitch_num, int mode_num);
int get_value_num(col_ptr c, int z, int y, int x);
char *get_value_name(int i);
char *vperm_pitches(char *str, col_ptr col, 
        int mode_num, int vperm_index, 
        int rperm_type, int rperm_index);
void vperm_print(col_ptr col);
void rperm_print_one(rperm_ptr rperm, int z);
void rperm_print(col_ptr col);
void col_print(col_ptr col);
void pinax_print(pinax_ptr p);
void music_print(char *str, pinax_ptr p, int col_num, 
        int mode_num, int vperm_index, int meter, int rperm_index);
 

/* MAIN */
int main(void) {
    arca_ptr kircher_ptr = &kircher;
    char output[MAX_STR];
    char *output_ptr = output;
    pinax_ptr p1_ptr = get_pinax_ptr(kircher_ptr, 0);
    
    col_ptr choice = get_col_ptr(p1_ptr, 0);
    output_ptr = vperm_pitches(output_ptr, choice, MODE1, 0, DUPLE, 0);
    printf("%s\n", output_ptr);

    music_print(output_ptr, p1_ptr, 4, MODE8, 8, TRIPLA_MINOR, 0);


/*    pinax_print(p1_ptr); */
    return(0);
}

/* FUNCTIONS */
pinax_ptr get_pinax_ptr(arca_ptr a, int i) {
    assert(a != NULL && i < a->max_pinax);
    return(a->pinax[i]);
}

col_ptr get_col_ptr(pinax_ptr p, int i) {
    assert(p != NULL && i < p->max_col);
    return(p->column[i]);
}

int get_pitch_num(col_ptr c, int z, int y, int x) {
    assert(c != NULL && 
            z < VPERM_Z && y < VPERM_Y && x < VPERM_X);
    return(c->vperm->array[z][y][x] - 1); 
    /* Subtract 1 because Kircher's numbers are 1-indexed */
}

char *get_note_name(int pitch_num, int mode_num) {
    int pitch_name_num;
    assert(pitch_num < MAX_PITCH && 
            mode_num >= MODE1 && mode_num < MAX_MODE);
    pitch_name_num = mode[mode_num][pitch_num];
    return(note_names[pitch_name_num]);
}

int get_value_num(col_ptr c, int z, int y, int x) {
    assert(c != NULL && 
            z < RPERM_Z && y < RPERM_Y && x < RPERM_X);
    return(c->rperm->array[z][y][x]);
}

char *get_value_name(int i) {
    assert(i < MAX_RHYTHM);
    return(rhythm_names[i]);
}

char *vperm_pitches(char *str, col_ptr col, 
        int mode_num, int vperm_index, 
        int rperm_type, int rperm_index) {
    int y, x;
    int pitch_num;
    char *note_name;
    int value_num;
    char *value_name;
    char tmp[MAX_STR];

    assert(col != NULL && sizeof(str) <= sizeof(tmp));

    str[0] = '\0';

    for (y = 0; y < VPERM_Y; ++y) {
        for (x = 0; x < col->syl; ++x) {
            pitch_num = get_pitch_num(col, vperm_index, y, x);
            note_name = get_note_name(pitch_num, mode_num);
            value_num = get_value_num(col, rperm_type, rperm_index, x);
            value_name = get_value_name(value_num);
            sprintf(tmp, "%s%s ", note_name, value_name);
            strcat(str, tmp);
        }
        strcat(str, "\n");
    }
    return(str);
}
    
void vperm_print(col_ptr col) {
    int x, y, z;
    int syl = col->syl;

    assert(col != NULL);

    for (z = 0; z < VPERM_Z; ++z) {
        for (y = 0; y < VPERM_Y; ++y) {
            for (x = 0; x < syl; ++x) {
                printf("%d ", get_pitch_num(col, z, y, x));
            }
            printf("\n");
        }
        printf("\n");
    }
    return;
}

void rperm_print_one(rperm_ptr rperm, int z) {
    int x, y, value;
    char *value_name;
    
    assert(rperm != NULL && z < RPERM_Z);
   
    for (y = 0; y < RPERM_Y && rperm->array[z][y][0] != 0; ++y) {
        for (x = 0; x < RPERM_X && rperm->array[z][y][x] != 0; ++x) {
            value = rperm->array[z][y][x];
            value_name = rhythm_names[value];
            printf("%s ", value_name);
        }
        printf("\n");
    }
    printf("\n");
    return;
}

void rperm_print(col_ptr col) {
    assert(col != NULL && col->rperm != NULL);

    printf("Notae correspondentes numeris Pinacis\n");
    rperm_print_one(col->rperm, 0);
    printf("Tripla maior\n");
    rperm_print_one(col->rperm, 1);
    printf("Tripla minor\n");
    rperm_print_one(col->rperm, 2);
    return;
}

void col_print(col_ptr col) {
    printf("%s.\n", roman[col->syl]);
    vperm_print(col);
    rperm_print(col);
    return;
}

void pinax_print(pinax_ptr p) {
    int i;
    printf("PINAX %s.\n", roman[p->id]);
    printf("%s\n\n", p->label);
    printf("%s\n\n", p->desc);
    for (i = 0; i < p->max_col; ++i) {
        col_print(p->column[i]);
    }
    return;
}

void music_print(char *str, pinax_ptr p, int col_num, 
        int mode_num, int vperm_index, int meter, int rperm_index) {
    col_ptr choice;
    assert(p != NULL);
    choice = get_col_ptr(p, col_num);
    str = vperm_pitches(str, choice, mode_num, vperm_index, meter, rperm_index);
    printf("%s\n", str);
    return;
}


