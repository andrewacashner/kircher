#ifndef ERROR_H
#define ERROR_H
/* error.h */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* Constants, lookup keys */
enum ERROR_CODE {
    DEFAULT,
    USAGE,
    BAD_METER,
    NO_INPUT_FILE,
    MODE_RANGE,
    FORBIDDEN_MODE,
    NO_COL_SYL,
    NO_RPERM,
    MAX_ERROR
};
extern enum ERROR_CODE error_code;

extern char *error_str[];

void exit_error(int code);

#endif
