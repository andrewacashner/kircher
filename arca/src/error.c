/* error.c */

#include "error.h"

void debug_print(char *fn_name, char *var_name, int var) {
#ifdef DEBUG
    printf("DEBUG %s: %s = %d\n", fn_name, var_name, var);
#endif
    return;
}

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

    assert(code >= 0);
    assert(code < MAX_ERROR);

    if (msg != NULL) {
        sprintf(stmt, ": %s", msg);
    } 
    fprintf(stderr, "Error -- %s%s\n", error_str[code], stmt);
    exit(EXIT_FAILURE);
}

