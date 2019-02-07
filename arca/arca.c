/* vim: set foldmethod=syntax : */

/* arca.c
 * Andrew A. Cashner
 * 2019/02/04
 * Tests toward a digital version of Kircher's Arca musarithmica
 */

#include <stdio.h>
#include <assert.h>

/* CONSTANTS, LABELS */

#define MAX_STR 100

/*      MODES (TONI) */
enum { nA, nBf, nB, nC, nCs, nD, nEf, nE, nF, nFs, nG, nGs } note_nums;
char *note_names[] = {
    "a", "bes", "b", "c", "cis", "d", "es", "e", "f", "fis", "g", "gis"
};

enum { 
    MODE1, MODE2, MODE3, MODE4, 
    MODE5, MODE6, MODE7, MODE8, 
    MODE9, MODE10, MODE11, MODE12 
} mode_names;

int mode[12][8] = {
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
    "r2"
};

char *roman[13] = {
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
    int max_cols;
    col_ptr *column;
} pinax;
typedef pinax *pinax_ptr;


/* DATA */

#include "data/pinax1.c"

/* FUNCTION PROTOTYPES */

void vperm_pitches(col_ptr col, int mode_num, int vperm_index, int rperm_type, int rperm_index);
void vperm_print(col_ptr col);
void rperm_print_one(rperm_ptr rperm, int z);
void rperm_print(col_ptr col);
void col_print(col_ptr col);

void pinax_print(pinax_ptr p);



/* MAIN */
int main(void) {
    pinax_ptr p1_ptr = &p1;
    
    /* pinax_print(p1_ptr); */

    vperm_pitches(p1_ptr->column[1], MODE2, 1, DUPLE, 0);

    return(0);
}


/* FUNCTIONS */
void vperm_pitches(col_ptr col, int mode_num, int vperm_index, int rperm_type, int rperm_index) {
    int y, x;
    int pitch_num;
    int pitch_name_num;
    char *note_name;
    int value_num;
    char *value_name;

    assert(col != NULL && 
            mode_num >= MODE1 && mode_num <= MODE12 && 
            vperm_index < VPERM_Z && 
            rperm_type < RPERM_Z && rperm_index < RPERM_Y);

    assert(col->rperm->array[rperm_type][rperm_index][0] != 0);

    for (y = 0; y < VPERM_Y; ++y) {
        for (x = 0; x < col->syl; ++x) {
            pitch_num = col->vperm->array[vperm_index][y][x] - 1;
            pitch_name_num = mode[mode_num][pitch_num];
            note_name = note_names[pitch_name_num];
            value_num = col->rperm->array[rperm_type][rperm_index][x];
            value_name = rhythm_names[value_num];
            printf("%s%s ", note_name, value_name);
        }
        printf("\n");
    }
    printf("\n");
    return;
}
    
void vperm_print(col_ptr col) {
    int x, y, z;
    int syl = col->syl;

    assert(col != NULL);

    for (z = 0; z < VPERM_Z; ++z) {
        for (y = 0; y < VPERM_Y; ++y) {
            for (x = 0; x < syl; ++x) {
                printf("%d ", col->vperm->array[z][y][x]);
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
    for (i = 0; i < p->max_cols; ++i) {
        col_print(p->column[i]);
    }
    return;
}
