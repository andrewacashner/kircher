/* syntagma1.c */
#include "main.h"
#include "syntagma1/syntagma1.h"

pinax_ptr pinaces[] = { &p1, &p2 };
pinax_index s1_pinax_index = {
    {
        {0, LONG},
        {1, SHORT}
    }
};
syntagma synt1 = { 2, pinaces, &s1_pinax_index };

syntagma_ptr syntagmata[] = { &synt1 };
arca kircher = { 1, syntagmata };
arca_ptr kircher_ptr = &kircher;
/* TODO this is in wrong place, can't be used in main */


