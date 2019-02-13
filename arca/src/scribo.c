/* scribo.c 
 * Arca module to take pre-scanned text and output music in Lilypond format 
 * Andrew A. Cashner
 * 2019/02/13
 */

#include "scribo.h"

char *voice_name[] = { "S", "A", "T", "B" };
char *clef_name[] = { "treble", "treble", "treble_8", "bass" };
char *rel_pitch[] = { "c\'\'", "c\'\'", "c\'", "c\'" };
char *ly_meter[] = { "4/2", "3/1", "3/2" };


void list_print_text(FILE *outfile, node_ptr ls) {
    if (ls != NULL) {
        fprintf(outfile, "%s\n", ls->text);
        list_print_text(ls->next);
    }
    return;
}

void print_lyrics(FILE *outfile, node_ptr ls) {
    fprintf(outfile, "\nLyrics = \\lyricmode {\n  ");
    list_print_text(outfile, ls);
    fprintf(outfile, "}\n\n");
    return;
}

void print_version(FILE *outfile, char *v_num) {
    fprintf(outfile, "\\version \"%s\"\n", v_num);
    return;
}

void print_voice_commands(FILE *outfile, int meter) {
    for (i = 0; i < MAX_VOICE; ++i) {
        sprintf(str, 
                "      \\new Staff\n"
                "      <<\n"
                "        \\new Voice = \"%s\" {\n"
                "           \\clef \"%s\"\n"
                "           \\relative %s {\n"
                "              \\time %s\n"
                "              \\Music%s\n"
                "           }\n"
                "        }\n"
                "        \\new Lyrics \\lyricsto \"%s\" { \\Lyrics }\n"
                "      >>\n",
                v_name,
                clef_name[i],
                rel_pitch[i],
                ly_meter[meter],
                v_name,
                v_name);
    }
    return;
}

void print_score(FILE *outfile, int meter) {
    fprintf(outfile, 
        "\\score {\n"
        "  <<\n"
        "    \\new ChoirStaff\n"
        "    <<\n");
    print_voice_commands(outfile, meter);
    fprintf(outfile, 
        "    >>\n"
        "  >>\n"
        "}\n");
    return;
}
/* TODO replace with string functions (linked lists of strings?) */

void print_music(FILE *outfile, music_node_ptr ls) {
    if (ls != NULL) {
        fprintf(outfile, "%s\n", ls->text);
        print_music(ls->next);
    }
    return;
}

void print_voices(FILE *outfile, music_node_ptr key) {
    if (key != NULL) {
        print_music(outfile, key->music_ls);
        print_voices(key->next);
    }
    return;
}

/* TODO build music_node list */
music_node_ptr music_create(node_ptr text, 
        syntagma_ptr syntagma, int mode, int meter) {
    music_node_ptr key = NULL;
    /* Create four linked lists, one per voice */
    /* Create one list of the four = key */
    return(key);
}

void compose(FILE *outfile, node_ptr text, 
        syntagma_ptr syntagma, int mode, int meter) {

    assert(ls != NULL && syntagma != NULL);

    music_node_ptr music_ls = music_create(text, syntagma, mode, meter);

    print_version(outfile, LY_VERSION);
    print_lyrics(outfile, text);
    print_voices(outfile, music_ls);
    print_score(outfile, meter);
    return;
}






