/* arca.c */
#include "arca.h"

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
    return(c->vperm->array[z][y][x] - 1); 
    /* Subtract 1 because Kircher's numbers are 1-indexed */
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
 

   
    


