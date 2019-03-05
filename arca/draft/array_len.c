#include <stdio.h>

#define END_STR_ARRAY "\0"

char *ly_include[] = {
    "ficta.ly",
    "automatic-ties.ly",
    END_STR_ARRAY
};

int str_array_len(char **str_array) {
    int i;
    for (i = 0; str_array[i][0] != '\0'; ++i) {
        ; /* just loop */
    }
    return(i);
}

void print_include(FILE *outfile, char **name_array) {
    int i;
    int max = str_array_len(name_array);
    for (i = 0; i < max; ++i) {
        fprintf(outfile, "\\include \"%s\"\n", name_array[i]);
    }
    return;
}

int main(void) {
    printf("length of array is %d\n", str_array_len(ly_include));
    return(0);
}
