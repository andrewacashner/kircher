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

node_ptr node_create(void) {
    node_ptr new = malloc(sizeof(node));
    return(new);
}

node_ptr node_set(node_ptr new, int syl, int len, char *str) {
    new->next = NULL;
    new->syllables = syl;
    new->penult_len = len;
    strcpy(new->text, str);

    return(new);
}

node_ptr last(node_ptr ls) {
    if (ls->next != NULL) {
        ls = last(ls->next);
    }
    return(ls);
}

node_ptr list_append(node_ptr ls, node_ptr new) {
    node_ptr head = ls;
    assert(new != NULL);
    if (ls == NULL) {
        head = new;
    } else {
        last(ls)->next = new;
    }
    return(head);
}

void list_free(node_ptr ls) {
    if (ls == NULL) {
        free(ls);
    } else {
        list_free(ls->next);
    }
    return;
}

void list_print_text(node_ptr ls) {
    if (ls != NULL) {
        printf("%s\n", ls->text);
        list_print_text(ls->next);
    }
    return;
}


node_ptr new_lyric_node(char *str) {
    int syl_count, syl_type;
    char syl_type_c;
    node_ptr new = node_create();
  
    syl_count = atoi(str);
    syl_type_c = str[2];
    syl_type = length_val(syl_type_c);
    new = node_set(new, syl_count, syl_type, &str[4]);
    return(new);
}

node_ptr text_list(node_ptr ls, FILE *infile) { 
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
                ls = list_append(ls, new_lyric_node(line));
                break;
        }
    }
    return(ls);
}

    /* TODO don't forget: list_free(ls); */








