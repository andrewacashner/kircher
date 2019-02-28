/* error.c */

#include "error.h"

void exit_error(int code, char *msg) {
    char *error_str[] = {
        "Unspecified",
        "Usage: arca -s <SYNTAGMA> -m <MODE> -t <TEMPUS> <INFILE> <OUTFILE>",
        "The tempus code on the command line is unacceptable",
        "Could not open input file for reading",
        "Could not open output file for reading",
        "The mode number is out of range",
        "The specified mode is not allowed with this pinax",
        "There is no column with the specified number of syllables",
        "There is no set of rhythmic values at the specified index"
    };

    char stmt[MAX_LINE] = "";

    check_range(code, 0, MAX_ERROR);

    if (msg != NULL) {
        sprintf(stmt, ": %s", msg);
    } 
    fprintf(stderr, "Error -- %s%s\n", error_str[code], stmt);
    exit(EXIT_FAILURE);
}

void check_ptr(void *ptr) {
    assert(ptr != NULL);
}

void check_range(int var, int min, int max) {
    /* Check var for min and max allowable values (<=, >=) */
    assert(var >= min && var <= max);
    return;
}

void check_voice_range(int voice_num) {
    check_range(voice_num, 0, 3);
    return;
}

void check_member(int var, int *values, int values_len) {
    /* Check if var is in list (array) of acceptable values */
    int i;
    bool found = false;

    for (i = 0; i < values_len; ++i) {
        if (var == values[i]) {
            break;
            found = true;
        }
    }
    assert(found == true);
    return;
}


