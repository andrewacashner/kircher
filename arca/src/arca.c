/* arca.c */
#include "arca.h"

char *note_names[] = {
    "a", "bes", "b", "c", "cis", 
    "d", "es", "e", "f", "fis", 
    "g", "gis", 
    "ERROR"
};

int mode_system[MAX_MODE] = {
    DURUS, MOLLIS, DURUS, DURUS, 
    MOLLIS, MOLLIS, DURUS, DURUS,
    MOLLIS, DURUS, DURUS, MOLLIS
};

/* toni = church keys? */
int mode[MAX_MODE][MAX_SCALE] = {
    { nD, nE, nF, nG, nA, nBf, nCs, nD },
    { nG, nA, nBf, nC, nD, nEf, nFs, nG },
    { nA, nB, nC, nD, nE, nF, nGs, nA },
    { nA, nB, nCs, nD, nE, nF, nG, nA },
    { nBf, nC, nD, nE, nF, nG, nA, nBf },
    { nF, nG, nA, nBf, nC, nD, nE, nF },
    { nG, nA, nB, nC, nD, nE, nFs, nG },
    { nG, nA, nB, nC, nD, nE, nFs, nG },
    { nD, nE, nF, nG, nA, nBf, nCs, nD },
    { nA, nBf, nCs, nD, nE, nF, nG, nA },
    { nC, nD, nE, nF, nG, nA, nB, nC },
    { nF, nG, nA, nBf, nC, nD, nE, nF }
};

char *rhythm_names[] = {
    " ",
    "\\breve. ",
    "\\breve ",
    "1. ",
    "1 ",
    "2. ",
    "2 ",
    "4. ",
    "4 ",
    "8 ",
    "r\\breve ",
    "r1 ",
    "r2 ",
    "r4 ",
    "ERROR"
};

char *roman[] = {
    "0",
    "I", "II", "III", "IV", "V", 
    "VI", "VII", "VIII", "IX", 
    "X", "XI", "XII"
};


/* FUNCTIONS */
int get_mode_pitch(int mode_num, int pitch_index) {
    return(mode[mode_num][pitch_index]);
}


syntagma_ptr get_syntagma_ptr(arca_ptr a, int i) {
    assert(a != NULL && i < a->max_syntagma);
    return(a->syntagma[i]);
}

pinax_ptr get_pinax_ptr(syntagma_ptr s, int i) {
    assert(s != NULL && i < s->max_pinax);
    return(s->pinax[i]);
}

pinax_ptr get_pinax_ptr_type(syntagma_ptr s, int penult_type) {
    int i, j;
    bool found = false;
    pinax_ptr pinax = NULL;
    assert(s != NULL);
    for (i = 0; i < s->max_pinax; ++i) {
        if (penult_type == s->p_index->array[i][0]) {
            j = s->p_index->array[i][1];
            found = true;
            break;
        }
    }
    if (found == true) {
        pinax = get_pinax_ptr(s, j);
    } 
    return(pinax);
}

col_ptr get_col_ptr(pinax_ptr p, int i) {
    assert(p != NULL && i < p->max_col);
    return(p->column[i]);
}

col_ptr get_col_ptr_syl(pinax_ptr p, int syl) {
    int i, j;
    bool found = false;
    col_ptr col;
    assert(p != NULL);
    for (i = 0; i < p->max_col; ++i) {
        /* Use syllable count to find correct column index */
        if (syl == p->col_syl_index->array[i][0]) {
            j = p->col_syl_index->array[i][1];
            found = true;
            break;
        }
    }
    if (found == true) {
        col = get_col_ptr(p, j);
    } else {
        col = NULL;
    }
    return(col);
}

int get_pitch_num(col_ptr c, int z, int y, int x) {
    assert(c != NULL && 
            z < VPERM_Z && y < VPERM_Y && x < VPERM_X);
    return(c->vperm->array[z][y][x]); 
    /* Subtract 1 because Kircher's numbers are 1-indexed */
}

char *get_note_name(int pitch_num, int mode_num) {
    /* Assume 0 index */
    int pitch_name_num;
    assert(pitch_num < MAX_PITCH);
    pitch_name_num = mode[mode_num][pitch_num];
    return(note_names[pitch_name_num]);
}

int get_value_num(col_ptr c, int z_rperm_type, int y_rperm_choice, int x_val) {
    int n;
    char coords[MAX_LINE];
    assert(c != NULL && 
            z_rperm_type < RPERM_Z && 
            y_rperm_choice < RPERM_Y && 
            x_val < RPERM_X);

    if (y_rperm_choice < c->rperm->bounds[z_rperm_type]) {
        n = c->rperm->array[z_rperm_type][y_rperm_choice][x_val];
    } else {
        sprintf(coords, "z %d/y %d/x %d", z_rperm_type, y_rperm_choice, x_val);
        exit_error(NO_RPERM, coords);
    }
    return(n);
}

char *get_value_name(int i) {
    assert(i < MAX_RHYTHM);
    return(rhythm_names[i]);
}

int check_mode(pinax_ptr p, int mode_num) {
    int i, retval = 0;
    bool found = false;

    assert(p != NULL);

    if (p->mode_blacklist[0] != ANY_MODE) {
        if (mode_num < 0 || mode_num >= MAX_MODE) {
            retval = MODE_RANGE; 
        } else {
            for (i = 0; i < MAX_MODE && p->mode_blacklist[i] != ARRAY_END; ++i) {
                if (mode_num == p->mode_blacklist[i]) {
                    found = true;
                    break;
                }
            }
            if (found == true) {
                retval = FORBIDDEN_MODE;
            } else {
                retval = 0;
            }
        }
    }
    return(retval);
}

int select_vperm(col_ptr col) {
    return(rand() % VPERM_X);
}

int select_rperm(col_ptr col, int meter) {
    int limit = col->rperm->bounds[meter];
    return(rand() % limit);
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
   
    for (y = 0; y < rperm->bounds[z] && y < RPERM_Y; ++y) {
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

void mode_print(int n) {
    printf("%d\n\n", n);
    return;
}


   
    


