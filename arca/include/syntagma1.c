/* syntagma1.c */

#include "syntagma1/pinax1.c"
#include "syntagma1/pinax2.c"

pinax_ptr pinaces[] = { &p1, &p2 };
pinax_index s1_pinax_index = {
    {
        {0, LONG},
        {1, SHORT}
    }
};
syntagma synt1 = { 2, pinaces ,&s1_pinax_index };

