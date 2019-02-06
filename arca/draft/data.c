/* DATA */

/* 2 syllables */
/*   voice perms */
int pinax1syl2[10][4][2] = {
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
};

/*   rhythm perms: duple */
int pinax1syl2val2[7][2] = {
    /* 0 */ { SB, SB },
    /* 1 */ { MN, MN },
    /* 2 */ { SM, SM },
    /* 3 */ { FS, FS },
    /* 4 */ { SBP, MN },
    /* 5 */ { MND, SM },
    /* 6 */ { SMD, FS }
};

/*    tripla maior */
int pinax1syl2val3[2][2] = {
    /* 0 */ { BR, SB },
    /* 1 */ { BRP, BRP }
};

/*   tripla menor */
int pinax1syl2val3m[2] = { SM, MN };


/* 3 syllables */
int pinax1syl3[10][4][3] = {
    { /* 0 */
        {3, 2, 3},
        {8, 7, 8},
        {5, 5, 5},
        {1, 5, 1}
    },
    { /* 1 */
        {4, 2, 8},
        {8, 7, 6},
        {6, 5, 3},
        {4, 5, 6}
    },
    { /* 2 */
        {4, 3, 4},
        {8, 8, 8},
        {6, 5, 5},
        {4, 1, 4}
    },
    { /* 3 */
        {2, 2, 2},
        {7, 6, 7},
        {5, 4, 5},
        {5, 2, 5}
    },
    { /* 4 */
        {6, 5, 5},
        {8, 7, 7},
        {4, 2, 2},
        {4, 5, 1}
    },
    { /* 5 */
        {5, 4, 3},
        {8, 8, 8},
        {3, 6, 5},
        {8, 4, 8}
    },
    { /* 6 */
        {5, 5, 5},
        {8, 7, 8},
        {3, 2, 3},
        {1, 5, 1}
    },
    { /* 7 */
        {6, 5, 6},
        {8, 8, 8},
        {4, 3, 4},
        {4, 8, 4}
    },
    { /* 8 */
        {3, 2, 3},
        {8, 7, 8},
        {5, 5, 5},
        {1, 5, 1}
    },
    { /* 9 */
        {6, 5, 5},
        {8, 7, 8},
        {4, 2, 3},
        {4, 5, 1}
    }
};
    
int pinax1syl4[10][4][4] = {
    { /* 0 */
        {5, 5, 5, 5},
        {7, 8, 7, 8},
        {2, 3, 2, 3},
        {5, 8, 5, 1}
    },
    { /* 1 */
        {3, 3, 2, 3},
        {8, 8, 7, 8},
        {5, 5, 5, 5},
        {8, 8, 5, 1}
    },
    { /* 2 */
        {3, 2, 7, 8},
        {8, 6, 5, 5},
        {5, 4, 2, 3},
        {3, 4, 5, 1}
    },
    { /* 3 */
        {5, 6, 5, 5},
        {8, 8, 7, 8},
        {3, 4, 2, 3},
        {8, 4, 5, 1}
    },
    { /* 4 */
        {6, 5, 6, 5},
        {8, 8, 8, 8},
        {4, 3, 4, 3},
        {4, 8, 4, 8}
    },
    { /* 5 */
        {2, 3, 2, 3},
        {7, 8, 7, 8},
        {5, 5, 5, 5},
        {5, 1, 5, 1}
    },
    { /* 6 */
        {3, 3, 2, 3},
        {8, 8, 7, 8},
        {5, 5, 5, 5},
        {8, 8, 5, 8}
    },
    { /* 7 */
        {3, 2, 7, 8},
        {8, 6, 5, 5},
        {5, 4, 2, 3},
        {3, 4, 5, 1}
    },
    { /* 8 */
        {2, 3, 2, 3},
        {7, 8, 7, 8},
        {5, 5, 5, 5},
        {5, 8, 5, 1}
    },
    { /* 9 */
        {4, 5, 6, 5},
        {8, 8, 8, 8},
        {6, 5, 4, 3},
        {4, 3, 4, 1}
    }
};

int pinax1syl5[10][4][5] = {
    { /* 0 */
        {2, 3, 3, 2, 3},
        {7, 8, 8, 7, 8},
        {5, 5, 5, 5, 5},
        {5, 3, 1, 5, 1}
    },
    { /* 1 */
        {5, 6, 6, 5, 5},
        {7, 8, 8, 8, 8},
        {3, 3, 4, 3, 4},
        {3, 6, 4, 8, 4}
    },
    { /* 2 */
        {3, 4, 3, 2, 3},
        {8, 8, 8, 8, 8},
        {5, 6, 5, 5, 5},
        {8, 4, 8, 5, 1}
    },
    { /* 3 */
        {2, 8, 2, 7, 8},
        {6, 5, 6, 5, 5},
        {4, 3, 2, 2, 3},
        {2, 3, 4, 5, 1}
    },
    { /* 4 */
        {5, 6, 5, 4, 3},
        {8, 8, 8, 8, 8},
        {3, 4, 5, 6, 5},
        {8, 4, 3, 4, 1}
    },
    { /* 5 */
        {2, 3, 3, 2, 3},
        {7, 8, 8, 7, 8},
        {5, 5, 5, 5, 5},
        {5, 3, 1, 5, 1}
    },
    { /* 6 */
        {4, 3, 2, 7, 8},
        {2, 8, 6, 5, 5},
        {6, 5, 4, 2, 3},
        {2, 3, 4, 5, 1}
    },
    { /* 7 */
        {5, 6, 5, 5, 5},
        {8, 8, 8, 7, 8},
        {3, 4, 3, 2, 3},
        {1, 4, 6, 5, 1}
    },
    { /* 8 */
        {3, 4, 4, 3, 4},
        {8, 8, 8, 8, 8},
        {5, 6, 6, 5, 6},
        {8, 6, 4, 1, 4}
    },
    { /* 9 */
        {5, 6, 5, 4, 3},
        {8, 8, 8, 8, 8},
        {3, 4, 5, 6, 5},
        {1, 4, 3, 4, 1}
    }
};

int pinax1syl6[10][4][6] = {
    {  /* 0 */
        {5, 5, 5, 3, 5, 5}, 
        {8, 8, 7, 8, 7, 8},
        {3, 3, 2, 2, 2, 3}, 
        {8, 8, 5, 6, 5, 1}
    },
    { /* 1 */
        {4, 4, 3, 2, 2, 2},
        {2, 2, 8, 7, 6, 7},
        {6, 6, 5, 5, 4, 5},
        {2, 2, 3, 5, 2, 5}
    },
    { /* 2 */
        {3, 2, 3, 5, 4, 5},
        {8, 7, 8, 2, 8, 2},
        {5, 5, 5, 2, 6, 7},
        {8, 5, 8, 7, 6, 5}
    },
    { /* 3 */
        {8, 8, 2, 3, 2, 3},
        {6, 5, 6, 8, 7, 8},
        {4, 5, 4, 5, 5, 5},
        {4, 3, 2, 1, 5, 1}
    },
    { /* 4 */
        {6, 5, 6, 5, 4, 3},
        {8, 8, 8, 8, 8, 8},
        {4, 3, 4, 5, 6, 5},
        {4, 8, 4, 3, 4, 1}
    },
    { /* 5 */
        {3, 8 ,7, 8, 7, 8},
        {5, 5, 5, 3, 5, 5},
        {3, 3, 2, 8, 2, 3},
        {8, 8, 5, 6, 5, 1}
    },
    { /* 6 */
        {4, 4, 3, 2, 2, 2},
        {2, 2, 8, 7, 6, 7},
        {6, 6, 5, 5, 4, 5},
        {2, 2, 3, 5, 2, 5}
    },
    { /* 7 */
        {3, 2, 3, 5, 4, 5},
        {8, 7, 8, 2, 8, 2},
        {5, 5, 5, 2, 6, 7},
        {8, 5, 8, 7, 6, 5}
    },
    { /* 8 */
        {6, 4, 3, 2, 2, 3},
        {8, 2, 8, 8, 7, 8},
        {4, 6, 5, 6, 5, 5},
        {4, 2, 3, 4, 5, 1}
    },
    { /* 9 */
        {6, 5, 6, 5, 4, 3},
        {8, 8, 8, 8, 8, 8},
        {4, 3, 4, 5, 6, 5},
        {4, 8, 4, 3, 4, 1}
    }
};

  

/* 3 syllables */ 
int pinax1syl3val2[7][5] = {
    /* 0 */ { SB, SB, SB, XX, XX },
    /* 1 */ { MN, SB, MN, XX, XX },
    /* 2 */ { rSB, rSM, SM, SB, SB },
    /* 3 */ { rSB, MN, SBP, MN, XX },
    /* 4 */ { SB, MN, MN, XX, XX },
    /* 5 */ { rSM, SM, SM, SM, XX },
    /* 6 */ { SM, MN, SM, XX, XX }
};

int pinax1syl3val3[2][4] = {
    /* 0 */ { SB, SB, SB, XX },
    /* 1 */ { rSB, SB, BR, SB }
};

int pinax1syl3val3m[2][4] = {
    /* 0 */ { MN, MN, MN, XX },
    /* 1 */ { rSB, MN, SB, MN }
};

/* 4 syllables */
int pinax1syl4val2[7][5] = {
    /* 0 */ { SB, SB, SB, SB, XX },
    /* 1 */ { SBP, MN, SB, SB, XX },
    /* 2 */ { MN, MN, SB, SB, XX },
    /* 3 */ { SM, SM, SB, MN, XX },
    /* 4 */ { rMN, SB, MN, SB, SB },
    /* 5 */ { rSM, MN, SM, MN, MN },
    /* 6 */ { MN, MN, MN, MN, XX}
};

int pinax1syl4val3[2][4] = {
    /* 0 */ { BR, SB, BR, SB },
    /* 1 */ { BR, SM, BRP, BRP }
};

int pinax1syl4val3m[2][4] = {
    /* 0 */ { SB, MN, SB, MN },
    /* 1 */ { SB, MN, SBP, SBP }
};

/* 5 syllables */
int pinax1syl5val2[8][6] = {
    /* 0 */ { SB, MN, MN, SB, SB, XX },
    /* 1 */ { MN, SB, MN, SB, SB, XX },
    /* 2 */ { SM, FS, FS, SM, SM, XX },
    /* 3 */ { SM, MN, SM, SB, SB, XX },
    /* 4 */ { MN, SM, SM, MN, MN, XX },
    /* 5 */ { rSM, SM, SM, SM, MN, MN },
    /* 6 */ { SB, MND, SM, MN, MN, XX },
    /* 7 */ { rMN, SB, MN, MN, MN, SB }
};

int pinax1syl5val3[2][6] = {
    /* 0 */ { SB, SB, SB, BR, SB, XX },
    /* 1 */ { rBR, SB, BR, SB, BR, SB }
};

int pinax1syl5val3m[2][6] = {
    /* 0 */ { MN, MN, MN, SB, MN, XX },
    /* 1 */ { rSB, MN, SB, MN, SBP, SBP }
};


/* 6 syllables */
int pinax1syl6val2[7][7] = {
    /* 0 */ { MN, MN, MN, MN, SB, SB, XX },
    /* 1 */ { SM, SM, SM, SM, MN, MN, XX },
    /* 2 */ { SBP, MN, MN, MN, SB, SB, XX },
    /* 3 */ { rSM, MN, SM, MN, MN, MN, MN },
    /* 4 */ { rMN, SB, MN, MN, MN, SB, SB },
    /* 5 */ { SM, SM, SMD, FS, MN, MN, XX },
    /* 6 */ { MND, SM, SM, SM, MN, SB, XX }
};

int pinax1syl6val3[3][7] = {
    /* 0 */ { BR, SB, BR, SB, BRP, BRP, XX },
    /* 1 */ { rSB, SB, SB, BR, SB, BR, SB },
    /* 2 */ { SB, SB, SB, SB, BR, BRP }
};

int pinax1syl6val3m[2][7] = {
    /* 0 */ { SB, MN, SB, MN, SBP, SBP, XX },
    /* 1 */ { rMN, MN, MN, SB, MN, SBP, SBP }
};


/* FUNCTION PROTOTYPES */
col_ptr col_init(col_ptr c, 
        int max_voice, int max_syllable, int *voices[10][4][6],
        int max_series_duple, int max_value_duple, int *values_duple[8][6],
        int max_series_triple, int max_value_triple, int *values_triple[3][7],
        int max_series_triple_minor, int max_value_triple_minor,
        int *values_triple_minor[2][7]);

pinax_ptr pinax_init(pinax_ptr p, int id, 
    char *title, char *description, 
    int *modes, col_ptr *column);

/* MAIN */
int main(void) {
    pinax pinax1;
    
    col pinax1col0, pinax1col1, pinax1col2, 
        pinax1col3, pinax1col4, pinax1col5;
    col_ptr pinax1cols[5]; 
    int pinax1modes[12] = { 1, 2, 3, 6, 7, 8, 9, 10, 11, 12};

    pinax1col0 = col_init(&pinax1col0, 
            4, 2, pinax1syl2,
            7, 2, &pinax1syl2val2,
            2, 2, &pinax1syl2val3,
            1, 2, &pinax1syl2val3m);
/*
    pinax1col1 = col_init(&pinax1col1, 
            4, 3, &pinax1syl3, 
            7, 5, &pinax1syl3val2,
            2, 4, &pinax1syl3val3,
            2, 4, &pinax1syl3val3m);

    pinax1col2 = col_init(&pinax1col2, 
            4, 4, &pinax1syl4, 
            7, 5, &pinax1syl4val2,
            2, 4, &pinax1syl4val3,
            2, 4, &pinax1syl5val3m);

    pinax1col3 = col_init(&pinax1col3, 
            4, 5, &pinax1syl5, 
            8, 6, &pinax1syl5val2,
            2, 6, &pinax1syl5val3,
            2, 6, &pinax1syl5val3m);

    pinax1col4 = col_init(&pinax1col4, 
            4, 6, &pinax1syl6, 
            7, 7, &pinax1syl6val2,
            3, 7, &pinax1syl6val3,
            2, 7, &pinax1syl6val3m);

    pinax1cols = {
        &pinax1col0, &pinax1col1, &pinax1col2, 
        &pinax1col3, &pinax1col4
    };

    pinax1 = pinax_init(&pinax1, 1, 
            "Melothesias siue Contrapunctisimplicis", 
            "Voces polysllabae, quae penultimam Longam habent",
            pinax1modes, pinax1cols);
*/
    return(0);
}



/* FUNCTIONS */
col_ptr col_init(col_ptr c, 
        int max_voice, int max_syllable, int *voices[10][4][6],
        int max_series_duple, int max_value_duple, int *values_duple[8][6],
        int max_series_triple, int max_value_triple, int *values_triple[3][7],
        int max_series_triple_minor, int max_value_triple_minor,
        int *values_triple_minor[2][7]) {

    c->max_syllable = max_syllable;
    c->max_voice = max_voice;
    c->voices = voices;

    c->max_series_duple = max_series_duple;
    c->max_value_duple = max_value_duple;
    c->values_duple = values_duple;

    c->max_series_triple = max_series_triple;
    c->max_value_triple = max_value_triple;
    c->values_triple = values_triple;

    c->max_series_triple_minor = max_series_triple_minor;
    c->max_value_triple_minor = max_value_triple_minor;
    c->values_triple_minor = values_triple_minor;

    return(c);
}

pinax_ptr pinax_init(pinax_ptr p, int id, 
    char *title, char *description, 
    int *modes, col_ptr *column) {

    p->id = id;
    p->title = title;
    p->description = description;
    p->modes = modes;
    p->column = column;

    return(p);
}



