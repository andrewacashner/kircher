/* arca.c
 * Andrew A. Cashner
 * 2019/02/04
 * 
 * A digital version of Athanasius Kircher's "Arca musarithmica"
 * for automatic music composition,
 * from *Musurgia universalis* (Rome, 1650) Bk. 8
 */

#include "main.h"

int main(int argc, char *argv[]) {
    int opt, syntagma, mode, tempus, tempus_index, meter;
    FILE *infile = NULL;
    FILE *outfile = NULL;
    char *infilename = NULL;
    char *outfilename = NULL;
   
    syntagma_ptr this_syntagma = NULL;
    textlist_ptr lyrics_ls = NULL;
    chorus_ptr composition = NULL;

    extern arca_ptr kircher_ptr; /* Defined in arca.c */

    composition = chorus_create();

    /* READ COMMAND-LINE OPTIONS */
    syntagma = mode = tempus = 0;
    while ((opt = getopt(argc, argv, "s:m:t:")) != -1) {
        switch (opt) {
            case 's':
                syntagma = atoi(optarg) - 1; 
                /* adjust for Kircher's 1-index */
                break;
            case 'm':
                mode = atoi(optarg) - 1;
                /* adjust for Kircher's 1-index */
                break;
            case 't':
                tempus = atoi(optarg);
                tempus_index = optind - 1;
                break;
            default:
                exit_error(USAGE, NULL);
                break;
            }
    }

    if (optind >= argc) {
        exit_error(USAGE, NULL);
    } 

    /* OPEN FILES */
    infilename = argv[optind];
    ++optind;
    outfilename = argv[optind];

    infile = fopen(infilename, "r");
    if (infile == NULL) {
        exit_error(NO_INPUT_FILE, infilename);
    }

    outfile = fopen(outfilename, "w");
    if (outfile == NULL) {
        exit_error(NO_OUTPUT_FILE, outfilename);
    }

    /* CHECK COMMAND-LINE OPTIONS AND SET VARIABLES */
    /* TODO test all command line settings for in range and format */
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
            exit_error(BAD_METER, argv[tempus_index]);
            break;
    }

    /* COMPOSE MUSIC */
    this_syntagma = get_syntagma_ptr(kircher_ptr, syntagma);
    lyrics_ls = text_list(lyrics_ls, infile);
    composition = chorus_compose(composition, lyrics_ls, this_syntagma, mode, meter);
/*    print_music(outfile, lyrics_ls, composition, mode, meter); */
    chorus_to_mei(outfile, composition);

    /* CLEAN UP */
    list_free(lyrics_ls);
    chorus_free(composition);

    fclose(infile);
    fclose(outfile);

    return(0);
}


