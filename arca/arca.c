/* vim: set foldmethod=syntax : */

/* arca.c
 * Andrew A. Cashner
 * 2019/02/04
 * Tests toward a digital version of Kircher's Arca musarithmica
 */

/* TODO
 * probably need to allocate memory dynamically to make data structures work
 * with matrices of different sizes
 */

#include <stdio.h>

/* CONSTANTS, LABELS */

#define MAX_STR 100

/*      MODES (TONI) */
enum { nA, nBf, nB, nC, nCs, nD, nEf, nE, nF, nFs, nG, nGs } note_nums;
char *note_names[] = {
    "a", "bes", "b", "c", "cis", "d", "es", "e", "f", "fis", "g", "gis"
};

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

char *rhythm_names[MAX_RHYTHM] = {
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
#define RPERM_Y 10
#define RPERM_X 10

/* DATA STRUCTURES */
typedef struct {
    int array[VPERM_Z][VPERM_Y][VPERM_X];
} vperm;
typedef vperm *vperm_ptr;

typedef struct {
    int array[RPERM_Y][RPERM_X];
} rperm;
typedef rperm *rperm_ptr;

typedef struct col {
    int syl;
    vperm_ptr vperm;
    rperm_ptr rperm2, rperm3, rperm3m;
} col;
typedef col *col_ptr;

/* DATA */

/* 2 syllables */
/*   voice perms */
vperm pinax1syl2v = {
    {
        { /* 0 */
            {5, 5},
            {7, 8},
            {2, 3},
            {5, 1}
        },
        { /* 1 */
            {5, 5},
            {7, 7},
            {2, 2},
            {5, 5}
        },
        { /* 2 */
            {5, 5},
            {8, 8},
            {3, 3},
            {8, 8}
        },
        { /* 3 */
            {4, 4},
            {6, 6},
            {8, 8},
            {4, 4}
        },
        { /* 4 */
            {4, 3},
            {8, 8},
            {6, 5},
            {4, 8}
        },
        { /* 5 */
            {3, 2},
            {8, 7},
            {5, 5},
            {8, 5}
        },
        { /* 6 */
            {5, 5},
            {8, 7},
            {3, 2},
            {8, 5}
        },
        { /* 7 */
            {5, 5},
            {7, 8},
            {2, 3},
            {5, 1}
        },
        { /* 8 */
            {2, 3},
            {7, 8},
            {5, 5},
            {5, 1}
        },
        { /* 9 */
            {6, 5},
            {8, 8},
            {4, 3},
            {4, 1}
        }
    }
};

/*   rhythm perms: duple */
rperm pinax1syl2val2 = {
    {
        /* 0 */ { SB, SB },
        /* 1 */ { MN, MN },
        /* 2 */ { SM, SM },
        /* 3 */ { FS, FS },
        /* 4 */ { SBP, MN },
        /* 5 */ { MND, SM },
        /* 6 */ { SMD, FS }
    }
};

/*    tripla maior */
rperm pinax1syl2val3 = {
    {
        /* 0 */ { BR, SB },
        /* 1 */ { BRP, BRP }
    }
};

/*   tripla menor */
rperm pinax1syl2val3m = { 
    {
        { SB, MN } 
    }
};


/* FUNCTION PROTOTYPES */
col_ptr col_init(col_ptr col, int syl, vperm_ptr vperm, 
        rperm_ptr rperm2, rperm_ptr rperm3, rperm_ptr rperm3m);

void vperm_pitches(col_ptr col, int mode_num, int vperm_index, int rperm_index);
void vperm_print(col_ptr col);
void rperm_print_one(rperm_ptr rperm);
void rperm_print(col_ptr col);
void col_print(col_ptr col);

/* MAIN */
int main(void) {
    col p1s2;
    col_ptr p1s2_ptr = col_init(&p1s2, 2, 
            &pinax1syl2v,
            &pinax1syl2val2,
            &pinax1syl2val3,
            &pinax1syl2val3m);

    col_print(p1s2_ptr);

    vperm_pitches(p1s2_ptr, 1, 0, 0);
    vperm_pitches(p1s2_ptr, 1, 0, 1);
    vperm_pitches(p1s2_ptr, 2, 1, 0);
    vperm_pitches(p1s2_ptr, 3, 5, 1);
    vperm_pitches(p1s2_ptr, 9, 9, 1);

    return(0);
}


/* FUNCTIONS */
col_ptr col_init(col_ptr col, int syl, vperm_ptr vperm, 
        rperm_ptr rperm2, rperm_ptr rperm3, rperm_ptr rperm3m) {
    col->syl = syl;
    col->vperm = vperm;
    col->rperm2 = rperm2;
    col->rperm3 = rperm3;
    col->rperm3m = rperm3m;
    return(col);
}

void vperm_pitches(col_ptr col, int mode_num, int vperm_index, int rperm_index) {
    int y, x;
    int pitch_num;
    int pitch_name_num;
    char *note_name;
    int value_num;
    char *value_name;
    for (y = 0; y < VPERM_Y; ++y) {
        for (x = 0; x < col->syl; ++x) {
            pitch_num = col->vperm->array[vperm_index][y][x] - 1;
            pitch_name_num = mode[mode_num][pitch_num];
            note_name = note_names[pitch_name_num];
            value_num = col->rperm2->array[rperm_index][x];
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
    printf("%s.\n", roman[syl]);
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

void rperm_print_one(rperm_ptr rperm) {
    int x, y, value;
    char *value_name;
    for (y = 0; y < RPERM_Y && rperm->array[y][0] != 0; ++y) {
        for (x = 0; x < RPERM_X && rperm->array[y][x] != 0; ++x) {
            value = rperm->array[y][x];
            value_name = rhythm_names[value];
            printf("%s ", value_name);
        }
        printf("\n");
    }
    printf("\n");
    return;
}

void rperm_print(col_ptr col) {
    printf("Notae correspondentes numeris Pinacis\n");
    rperm_print_one(col->rperm2);
    printf("Tripla maior\n");
    rperm_print_one(col->rperm3);
    printf("Tripla minor\n");
    rperm_print_one(col->rperm3m);
    return;
}

void col_print(col_ptr col) {
    vperm_print(col);
    rperm_print(col);
    return;
}
