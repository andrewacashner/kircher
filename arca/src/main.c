/* vim: set foldmethod=syntax : */

/* arca.c
 * Andrew A. Cashner
 * 2019/02/04
 * 
 * A digital version of Athanasius Kircher's "Arca musarithmica"
 * for automatic music composition,
 * from *Musurgia universalis* (Rome, 1650) Bk. 8
 */

/* TODO
 * check data: rhythm values: rests
 * add octaves to notes and/or voices/clefs
 */

#include "main.h"

int main(int argc, char *argv[]) {
    int opt, syntagma, mode, tempus, meter;
    FILE *infile = NULL;
    char *infilename = NULL;
    syntagma_ptr this_syntagma = NULL;
    node_ptr ls = NULL;

    syntagma = mode = tempus = 0;
    while ((opt = getopt(argc, argv, "s:m:t:")) != -1) {
        switch (opt) {
            case 's':
                syntagma = atoi(optarg) - 1; 
                /* adjust for Kircher's 1-index */
                break;
            case 'm':
                mode = atoi(optarg);
                /* But our modes ARE 1-indexed TODO */
                break;
            case 't':
                tempus = atoi(optarg);
                break;
            default:
                exit_error(USAGE);
                break;
            }
    }

    switch (tempus) {
        case 2:
            meter = DUPLE;
            break;
        case 32:
            meter = TRIPLE;
            break;
        case 3:
            meter = TRIPLE_M;
            break;
        default:
            exit_error(BAD_METER);
    }

    if (optind >= argc) {
        exit_error(USAGE);
    } else {
        infilename = argv[optind];
    }

    infile = fopen(infilename, "r");
    if (infile == NULL) {
        exit_error(NO_INPUT_FILE);
    }

    this_syntagma = get_syntagma_ptr(kircher_ptr, syntagma);

    ls = text_list(ls, infile);

    list_print_text(ls);
    printf("\n");

    mode_print(mode);

    while (ls != NULL) {
        music_print(this_syntagma, mode, meter, ls);
        ls = ls->next;
    }

    list_free(ls);
    fclose(infile);
    return(0);
}


