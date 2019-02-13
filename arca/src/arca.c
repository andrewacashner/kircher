/* arca.c */
#include "arca.h"

char *note_names[] = {
    "a", "bes", "b", "c", "cis", 
    "d", "es", "e", "f", "fis", 
    "g", "gis", 
    "ERROR"
};

int mode[][MAX_SCALE] = {
    { NONE },
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


/* FUNCTIONS */
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
    int pitch_name_num;
    assert(pitch_num < MAX_PITCH);
    --pitch_num; /* Adjust for Kircher's 1-indexed numbers */
    pitch_name_num = mode[mode_num][pitch_num];
    return(note_names[pitch_name_num]);
}

int get_value_num(col_ptr c, int z_rperm_type, int y_rperm_choice, int x_val) {
    int n;
    assert(c != NULL && 
            z_rperm_type < RPERM_Z && 
            y_rperm_choice < RPERM_Y && 
            x_val < RPERM_X);

    if (y_rperm_choice < c->rperm->bounds[z_rperm_type]) {
        n = c->rperm->array[z_rperm_type][y_rperm_choice][x_val];
    } else {
        exit_error(NO_RPERM);
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

    if (p->mode_blacklist[0] != ANY) {
        if (mode_num < 0 || mode_num >= MAX_MODE) {
            retval = MODE_RANGE; 
        } else {
            for (i = 0; found == false && i < MAX_MODE ; ++i) {
                if (mode_num == p->mode_blacklist[i]) {
                    found = true;
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
 
char *vperm_pitches(char *str, col_ptr col, 
        int mode_num, int vperm_index, 
        int rperm_type, int rperm_index) {
    int y, x, r;
    int pitch_num;
    char *note_name;
    int value_num;
    char *value_name;

    assert(col != NULL && sizeof(str) <= sizeof(char)*MAX_CHAR);

    strcpy(str, "");

    for (y = 0; y < VPERM_Y; ++y) {
        r = x = 0;
        while (r < RPERM_X && x < col->syl) {
            /* Get just the rhythm if it is a rest;
             * if so move to next rhythm but keep same pitch */
            value_num = get_value_num(col, rperm_type, rperm_index, r);
            value_name = get_value_name(value_num);
            if (value_num < MIN_REST) {
                /* Rhythm != rest, print pitch + rhythm, move to next */
                pitch_num = get_pitch_num(col, vperm_index, y, x);
                note_name = get_note_name(pitch_num, mode_num);
                strcat(str, note_name);
                ++x, ++r;
            } else {
                /* Rhythm is rest, just print rhythm and match current pitch (x)
                 * to next rhythm (r) */
                ++r;
            }
            strcat(str, value_name);
            strcat(str, " ");
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

/* TODO replace with linked list so all music for each voice is separate */
void music_print(syntagma_ptr syntagma, int mode, int meter, node_ptr node) {
    int test, vperm_index, rperm_index, syllables, penult_len;
    char str[MAX_CHAR]; /* TODO unneeded? */
    pinax_ptr pinax = NULL;
    col_ptr col = NULL;

    assert(syntagma != NULL && node != NULL);

    syllables = node->syllables;
    penult_len = node->penult_len;
   
    pinax = get_pinax_ptr_type(syntagma, penult_len);

    test = check_mode(pinax, mode);
    if (test != 0) {
        exit_error(test);
    }

    col = get_col_ptr_syl(pinax, syllables);
    vperm_index = select_vperm(col);
    rperm_index = select_rperm(col, meter);

    if (col != NULL) {
        strcpy(str, vperm_pitches(str, col, mode,
                    vperm_index, meter, rperm_index));
        printf("%s\n", str);
    } else {
        exit_error(NO_COL_SYL);
    }
    return;
}

   
    


