#ifndef ERROR_H
#define ERROR_H
/* error.h */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#define MAX_LINE 160

enum ERROR_CODE {
    DEFAULT,
    USAGE,
    BAD_METER,
    NO_INPUT_FILE,
    NO_OUTPUT_FILE,
    MODE_RANGE,
    FORBIDDEN_MODE,
    NO_COL_SYL,
    NO_RPERM,
    MAX_ERROR
};
extern enum ERROR_CODE error_code;

void debug_print(char *fn_name, char *var_name, int var);

void exit_error(int code, char *msg);

#endif
