/* error.c */

#include "error.h"

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

void exit_error(int code) {
    assert(code < MAX_ERROR);
    fprintf(stderr, "Error: %s.\n", error_str[code]);
    exit(EXIT_FAILURE);
}



