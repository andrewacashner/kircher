/* arca.h */
#ifndef ARCA_H
#define ARCA_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "error.h"
#include "lectio.h"

/* CONSTANTS, LABELS */
#define MAX_CHAR 256*100

/* Array bounds */
#define VPERM_Z 10
#define VPERM_Y 4
#define VPERM_X 10

#define RPERM_Z 3
#define RPERM_Y 10
#define RPERM_X 10

#define MAX_COL 5
#define MAX_PINAX 2 /* TODO for now */

#define MAX_SCALE 8
#define MAX_MODE 13

/* MODES (TONI) */
enum NOTE_NUMS { 
    nA, nBf, nB, nC, nCs, 
    nD, nEf, nE, nF, nFs, 
    nG, nGs, 
    MAX_PITCH 
};
extern enum NOTE_NUMS note_nums;

extern char *note_names[];

enum MODE_NAMES { 
    NONE,
    MODE1, MODE2, MODE3, MODE4, 
    MODE5, MODE6, MODE7, MODE8, 
    MODE9, MODE10, MODE11, MODE12,
    ANY
};
extern enum MODE_NAMES mode_names;

extern int mode[][MAX_SCALE];

/* RHYTHMS */
enum RPERM_TYPE { 
    DUPLE, TRIPLE, TRIPLE_M 
};
extern enum RPERM_TYPE rperm_type;

enum RHYTHMIC_VALUES { 
    XX,     /* no value, blank */
    BRD,    /* breve dotted (= perfect) */
    BR,     /* breve imperfect */
    SBD,    /* semibreve dotted (= perfect) */
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
};
extern enum RHYTHMIC_VALUES rhythmic_values;

enum REST_BOUNDS { MIN_REST = rBR };
extern enum REST_BOUNDS rest_bounds;

extern char *rhythm_names[];
extern char *roman[];

/* Type of penultimate syllable */
enum SYL_TYPE_CODE { 
    SHORT, LONG, MAX_SYL_TYPE 
};
extern enum SYL_TYPE_CODE syl_type_code;


/* DATA STRUCTURES */
typedef struct {
    int array[VPERM_Z][VPERM_Y][VPERM_X];
} vperm;
typedef vperm *vperm_ptr;

typedef struct {
    int bounds[3]; /* index using rperm_type enum */
    int array[RPERM_Z][RPERM_Y][RPERM_X];
} rperm;
typedef rperm *rperm_ptr;

typedef struct col {
    int syl;
    vperm_ptr vperm;
    rperm_ptr rperm;
} col;
typedef col *col_ptr;

typedef struct col_index {
    /* 2D array matches number of syllables to column index,
     * e.g., if column index 0 has 2 syllables, { {2, 0} } */
    int array[MAX_COL][2];
} col_index;
typedef col_index *col_index_ptr;

typedef struct pinax {
    int id;
    char *label;
    char *desc;
    int mode_blacklist[MAX_MODE]; 
        /* Modes that should not be used with this pinax. Use NULL if all modes
         * can be used. */
    int max_col;
    col_ptr *column;
    col_index_ptr col_syl_index;
} pinax;
typedef pinax *pinax_ptr;


typedef struct pinax_index {
    /* same deal as col_index but for types of penultimate syllable (long or
     * short)
     */
    int array[MAX_PINAX][2];
} pinax_index;
typedef pinax_index *pinax_index_ptr;

typedef struct syntagma {
    int max_pinax;
    pinax_ptr *pinax;
    pinax_index_ptr p_index;
} syntagma;
typedef syntagma *syntagma_ptr;

typedef struct arca {
    int max_syntagma;
    syntagma_ptr *syntagma;
} arca;
typedef arca *arca_ptr;

/* VARIABLE DECLARATIONS */
extern arca kircher;
extern arca_ptr kircher_ptr;

/* FUNCTION PROTOTYPES */

void exit_error(int code);

pinax_ptr get_pinax_ptr(syntagma_ptr s, int i);
pinax_ptr get_pinax_ptr_type(syntagma_ptr s, int penult_type);
syntagma_ptr get_syntagma_ptr(arca_ptr a, int i);
col_ptr get_col_ptr(pinax_ptr p, int i);
col_ptr get_col_ptr_syl(pinax_ptr p, int syl);

int check_mode(pinax_ptr p, int mode_num);
int get_pitch_num(col_ptr c, int z, int y, int x);
char *get_note_name(int pitch_num, int mode_num);
int get_value_num(col_ptr c, int z, int y, int x);
char *get_value_name(int i);
int select_rperm(col_ptr col, int meter);
int select_vperm(col_ptr col);
void vperm_print(col_ptr col);
void rperm_print_one(rperm_ptr rperm, int z);
void rperm_print(col_ptr col);
void col_print(col_ptr col);
void pinax_print(pinax_ptr p);
void mode_print(int n);

#endif 
