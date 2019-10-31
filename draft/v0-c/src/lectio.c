#include "lectio.h"

/* FUNCTIONS */ 
int length_val(char c) {
    int i;
    char *syl_char = "SL";
    for (i = 0; syl_char[i] != '\0'; ++i) {
        if (syl_char[i] == c) {
            break;
        }
    }
    return(i);
}

textlist_ptr textlist_create(void) {
    textlist_ptr new = malloc(sizeof(textlist));
    return(new);
}

textlist_ptr textlist_set(textlist_ptr new, int syl, int len, char *str) {
    new->next = NULL;
    new->syllables = syl;
    new->penult_len = len;
    strcpy(new->text, str);

    return(new);
}

textlist_ptr last(textlist_ptr ls) {
    if (ls->next != NULL) {
        ls = last(ls->next);
    }
    return(ls);
}

textlist_ptr list_append(textlist_ptr ls, textlist_ptr new) {
    textlist_ptr head = ls;
    assert(new != NULL);
    if (ls == NULL) {
        head = new;
    } else {
        last(ls)->next = new;
    }
    return(head);
}

void list_free(textlist_ptr ls) {
    if (ls == NULL) {
        free(ls);
    } else {
        list_free(ls->next);
    }
    return;
}

textlist_ptr new_textlist(char *str) {
    int syl_count, syl_type;
    char syl_type_c;
    textlist_ptr new = textlist_create();
  
    syl_count = atoi(str);
    syl_type_c = str[2];
    syl_type = length_val(syl_type_c);
    new = textlist_set(new, syl_count, syl_type, &str[4]);
    return(new);
}

textlist_ptr text_list(textlist_ptr ls, FILE *infile) { 
    char line[MAX_CHAR];

    assert(infile != NULL);

    while (fgets(line, sizeof(line), infile) != NULL) {
        switch (line[0]) {
            case '%':
                /* Comment line, ignore */
                continue;
            case '\n':
                /* Blank line, ignore */
                continue;
            default:
                line[strlen(line) - 1] = '\0';
                ls = list_append(ls, new_textlist(line));
                break;
        }
    }
    return(ls);
}








