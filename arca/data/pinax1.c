/* vim: set foldmethod=syntax : */

/* Kircher arca data *
 *
 * p1s2 = pinax 1, 2 syllables
 * vperm = voice permutations ("musarithmi")
 * rperm = rhythm permutations: duple, tripla maior, tripla minor
 */

/* PINAX 1 */

/***********************************************
 * 2 syllables
 ***********************************************/
vperm p1s2v = {
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

rperm p1s2r = {
    { 
        { /* duple */
            /* 0 */ { SB, SB },
            /* 1 */ { MN, MN },
            /* 2 */ { SM, SM },
            /* 3 */ { FS, FS },
            /* 4 */ { SBP, MN },
            /* 5 */ { MND, SM },
            /* 6 */ { SMD, FS }
        },
        { /* tripla maior */

            /* 0 */ { BR, SB },
            /* 1 */ { BRP, BRP }
        },
        { /*   tripla menor */
            { SB, MN } 
        }
    }
};

col p1s2 = { 2, &p1s2v, &p1s2r };

/***********************************************
 * 3 syllables
 ***********************************************/
vperm p1s3v = {
    {
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
    }
};

rperm p1s3r = {
    {
        { /* duple */
            /* 0 */ { SB, SB, SB },
            /* 1 */ { MN, SB, MN },
            /* 2 */ { rSB, rSM, SM, SB, SB },
            /* 3 */ { rSB, MN, SBP, MN },
            /* 4 */ { SB, MN, MN },
            /* 5 */ { rSM, SM, SM, SM },
            /* 6 */ { SM, MN, SM }
        },
        { /* tripla maior */
            /* 0 */ { SB, SB, SB },
            /* 1 */ { rSB, SB, BR, SB }
        },
        { /* tripla minor */
            /* 0 */ { MN, MN, MN },
            /* 1 */ { rSB, MN, SB, MN }
        }
    }
};

col p1s3 = { 3, &p1s3v, &p1s3r };

/***********************************************
 * 4 syllables
 ***********************************************/

vperm p1s4v = {
    {
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
    }
};

rperm p1s4r = {
    {
        { /* duple */
            /* 0 */ { SB, SB, SB, SB },
            /* 1 */ { SBP, MN, SB, SB },
            /* 2 */ { MN, MN, SB, SB },
            /* 3 */ { SM, SM, SB, MN },
            /* 4 */ { rMN, SB, MN, SB, SB },
            /* 5 */ { rSM, MN, SM, MN, MN },
            /* 6 */ { MN, MN, MN, MN }
        },
        { /* tripla maior */
            /* 0 */ { BR, SB, BR, SB },
            /* 1 */ { BR, SM, BRP, BRP }
        },
        { /* tripla minor */
            /* 0 */ { SB, MN, SB, MN },
            /* 1 */ { SB, MN, SBP, SBP }
        }
    }
};

col p1s4 = { 4, &p1s4v, &p1s4r };

/***********************************************
 * 5 syllables
 ***********************************************/
vperm p1s5v = {
    {
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
    }
};

rperm p1s5r = {
    {
        { /* duple */
            /* 0 */ { SB, MN, MN, SB, SB },
            /* 1 */ { MN, SB, MN, SB, SB },
            /* 2 */ { SM, FS, FS, SM, SM },
            /* 3 */ { SM, MN, SM, SB, SB },
            /* 4 */ { MN, SM, SM, MN, MN },
            /* 5 */ { rSM, SM, SM, SM, MN, MN },
            /* 6 */ { SB, MND, SM, MN, MN },
            /* 7 */ { rMN, SB, MN, MN, MN, SB }
        },
        { /* tripla maior */
            /* 0 */ { SB, SB, SB, BR, SB },
            /* 1 */ { rBR, SB, BR, SB, BR, SB }
        },
        { /* tripla minor */
            /* 0 */ { MN, MN, MN, SB, MN },
            /* 1 */ { rSB, MN, SB, MN, SBP, SBP }
        }
    }
};

col p1s5 = { 5, &p1s5v, &p1s5r };

/***********************************************
 * 6 syllables
 ***********************************************/
vperm p1s6v = {
    {
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
    }
};

rperm p1s6r = {
    {
        { /* duple */
            /* 0 */ { MN, MN, MN, MN, SB, SB },
            /* 1 */ { SM, SM, SM, SM, MN, MN },
            /* 2 */ { SBP, MN, MN, MN, SB, SB },
            /* 3 */ { rSM, MN, SM, MN, MN, MN, MN },
            /* 4 */ { rMN, SB, MN, MN, MN, SB, SB },
            /* 5 */ { SM, SM, SMD, FS, MN, MN },
            /* 6 */ { MND, SM, SM, SM, MN, SB }
        },
        { /* tripla maior */
            /* 0 */ { BR, SB, BR, SB, BRP, BRP },
            /* 1 */ { rSB, SB, SB, BR, SB, BR, SB },
            /* 2 */ { SB, SB, SB, SB, BR, BRP }
        },
        { /* tripla minor */
            /* 0 */ { SB, MN, SB, MN, SBP, SBP },
            /* 1 */ { rMN, MN, MN, SB, MN, SBP, SBP }
        }
    }
};

col p1s6 = { 6, &p1s6v, &p1s6r };

col_ptr p1cols[] = { &p1s2, &p1s3, &p1s4, &p1s5, &p1s6 };

pinax p1 = {
    1,
    "Melothesias siue Contrapuncti simplicis",
    "Voces Polysyllabae, quae penultimam Longam habent",
    5,
    p1cols
};

