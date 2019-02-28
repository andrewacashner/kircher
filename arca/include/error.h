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

void exit_error(int code, char *msg);

void check_ptr(void *ptr);
void check_range(int var, int min, int max);
void check_voice_range(int voice_num);
void check_member(int var, int *values, int values_len);

#endif
