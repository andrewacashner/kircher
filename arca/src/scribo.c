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

music_node_ptr music_node_create(void) {
    music_node_ptr new = malloc(sizeof(music_node));
    return(new);
}

music_node_ptr music_node_set(music_node_ptr node, 
        music_node_ptr next, char *text) {

    assert(node != NULL);
    node->next = next;
    strcpy(node->text, text);
    return(node);
}

music_node_ptr last_music(music_node_ptr ls) {
    if (ls != NULL) {
        last_music(ls->next);
    }
    return(ls);
}

music_node_ptr music_ls_append(music_node_ptr ls, music_node_ptr node) {
    music_node_ptr head = ls;
    assert(node != NULL);
    if (ls == NULL) {
        head = node;
    } else {
        last_music(ls)->next = node;
    }
    return(head);
}

typedef enum VOICE_NAMES {
    CANTUS, ALTUS, TENOR, BASSUS
};
extern enum VOICE_NAMES voice_names;

music_node_ptr *music_list(music_node_ptr *music, node_ptr lyrics_ls, 
        syntagma_ptr syntagma, int mode, int meter) {

    int i, syllables, penult_length;
    int test, vperm_index, rperm_index;
    pinax_ptr pinax = NULL;
    col_ptr col = NULL;
    music_node_ptr cantus = NULL;
    music_node_ptr altus  = NULL;
    music_node_ptr tenor = NULL;
    music_node_ptr bassus = NULL;
    music_node_ptr chorus[] = { cantus, altus, tenor, bassus };
    music_node_ptr *chorus_ptr = chorus;

    assert(lyrics_ls != NULL && syntagma != NULL);

    while (lyrics_ls != NULL) {
        syllables = lyrics_ls->syllables;
        penult_length = lyrics_ls->penult_length;
       
        pinax = get_pinax_ptr_type(syntagma, penult_length);
        
        test = check_mode(pinax, mode);
        if (test != 0) {
            exit_error(test);
        }

        col = get_col_ptr_syl(pinax, syllables);
        vperm_index = select_vperm(col);
        rperm_index = select_rperm(col, meter);

        if (col == NULL) {
            exit_error(NO_COL_SYL);
        }

        for (i = 0; i < 4; ++i) {
            chorus[i] = compose(chorus[i], i, 
                col, vperm_index, meter, rperm_index)
        }
        lyrics_ls = lyrics_ls->next;
    }

    music = chorus_ptr;
    return(music);
}

music_node_ptr compose(music_node_ptr music_ls, int voice_num,
        col_ptr col, int mode, int vperm_index, 
        int rperm_type, int rperm_index) {

    int x, r;
    int pitch_num, value_num;
    char *note_name, *value_name;

    music_node_ptr new = music_node_create();

    r = x = 0;
    while (r < RPERM_X && x < col->syl) {
        /* Get just the rhythm if it is a rest;
         * if so move to next rhythm but keep same pitch */
        value_num = get_value_num(col, rperm_type, rperm_index, r);
        value_name = get_value_name(value_num);
        if (value_num < MIN_REST) {
            /* Rhythm != rest, print pitch + rhythm, move to next */
            pitch_num = get_pitch_num(col, vperm_index, voice_num, x);
            note_name = get_note_name(pitch_num, mode);
            strcat(str, note_name);
            ++x, ++r;
        } else {
            /* Rhythm is rest, just print rhythm and match current pitch (x)
             * to next rhythm (r) */
            ++r;
        }
        new = music_node_set(new, NULL, value_name);
    }

    music_ls_append(music_ls, new);
    return(music_ls);
}
    
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

void print_voices(FILE *outfile, music_node_ptr *chorus, int voice) {
    if (voice < 4) {
        music_list_print(outfile, key[voice]);
        ++voice;
        print_voices(outfile, key, voic);
    }
    return;
}

void print_music(FILE *outfile, node_ptr text, 
        music_node_ptr *music, int meter) {
    print_version(outfile, LY_VERSION);
    print_lyrics(outfile, text);
    print_voices(outfile, music, 0);
    print_score(outfile, meter);
    return;
}







