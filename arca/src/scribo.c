/* scribo.c 
 * Arca module to take pre-scanned text and output music in Lilypond format 
 * Andrew A. Cashner
 * 2019/02/13
 */

#include "scribo.h"
#include "interval.h"

char *voice_name[] = { "S", "A", "T", "B" };
char *clef_name[] = { "treble", "treble", "treble_8", "bass" };
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
    if (ls->next != NULL) {
        ls = last_music(ls->next);
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

chorus_ptr chorus_create(chorus_ptr chorus) {
    chorus->cantus = music_node_create();
    chorus->altus = music_node_create();
    chorus->tenor = music_node_create();
    chorus->bassus = music_node_create();
    return(chorus);
}

music_node_ptr select_voice(chorus_ptr chorus, int n) {
    music_node_ptr tmp;
    switch (n) {
        case CANTUS: return(chorus->cantus);
        case ALTUS:  return(chorus->altus);
        case TENOR:  return(chorus->tenor);
        case BASSUS: return(chorus->bassus);
        default:     return(NULL);
    }
    return(tmp);
}

/* TODO Is there a blank node at the front of the list? */
chorus_ptr music_create(chorus_ptr chorus, node_ptr lyrics_ls, 
        syntagma_ptr syntagma, int mode, int meter) {

    int i, syllables, penult_len;
    int test, vperm_index, rperm_index;
    pinax_ptr pinax = NULL;
    col_ptr col = NULL;
    music_node_ptr voice = NULL;
    char error_msg[MAX_LINE];
    int seed = time(NULL);
    srand(seed);

    assert(lyrics_ls != NULL && syntagma != NULL);

    while (lyrics_ls != NULL) {
        syllables = lyrics_ls->syllables;
        penult_len = lyrics_ls->penult_len;
       
        pinax = get_pinax_ptr_type(syntagma, penult_len);
        
        test = check_mode(pinax, mode);
        if (test != 0) {
            sprintf(error_msg, "%d", mode + 1); /* back to 1-index */
            exit_error(test, error_msg);
        }

        col = get_col_ptr_syl(pinax, syllables);
        vperm_index = select_vperm(col);
        rperm_index = select_rperm(col, meter);

        if (col == NULL) {
            sprintf(error_msg, "%d", syllables);
            exit_error(NO_COL_SYL, error_msg);
        }

        for (i = 0; i < MAX_VOICE; ++i) {
            voice = select_voice(chorus, i);
            voice = compose(voice, i, col, mode, 
                    vperm_index, meter, rperm_index);
        }
        lyrics_ls = lyrics_ls->next;
    }
    return(chorus);
}
music_node_ptr compose(music_node_ptr music_ls, int voice_num,
        col_ptr col, int mode, int vperm_index, 
        int rperm_type, int rperm_index) {
    
    int x, r, octave, tick, t;
    int pitch_num, value_num;
    char *note_name, *value_name;
    char octave_mark[10];
    music_node_ptr new = music_node_create();
    musarithm_ptr mus = musarithm_create();
    
    new->next = NULL;
    mus = musarithm_set(mus, col, vperm_index, mode);

    r = x = 0;
    while (r < RPERM_X && x < col->syl) {
         /* Get just the rhythm if it is a rest;
         * if so move to next rhythm but keep same pitch */
        value_num = get_value_num(col, rperm_type, rperm_index, r);
        value_name = get_value_name(value_num);
        if (value_num < MIN_REST) {
            /* Rhythm != rest, print pitch + rhythm, move to next */
            pitch_num = mus_get_pitch_class(mus, voice_num, x);
            note_name = get_note_name(pitch_num, mode);
            strcat(new->text, note_name);
            /* Add octave tick marks */
            octave = mus_get_octave(mus, voice_num, x);
            tick = octave_ticks(octave);
            if (tick != 0) {
                if (tick < 0) {
                    strcpy(octave_mark, ",");
                    tick *= -1;
                } else if (tick > 0) {
                    strcpy(octave_mark, "\'");
                }
                for (t = 0; t < tick; ++t) {
                    strcat(new->text, octave_mark);
                }
            }
            ++x, ++r;
        } else {
            /* Rhythm is rest, just print rhythm and match current pitch (x)
             * to next rhythm (r) */
            ++r;
        }
        strcat(new->text, value_name);
    }
    music_ls = music_ls_append(music_ls, new);
    return(music_ls);
}
    
void list_print_text(FILE *outfile, node_ptr ls) {
    if (ls != NULL) {
        fprintf(outfile, "%s\n", ls->text);
        list_print_text(outfile, ls->next);
    }
    return;
}

void list_print_music(FILE *outfile, music_node_ptr ls) {
    if (ls != NULL) {
        fprintf(outfile, "%s\n", ls->text);
        list_print_music(outfile, ls->next);
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

void print_voice_commands(FILE *outfile, int mode, int meter) {
    int i;
    char *v_name; 
    int mode_system[] = {
        DURUS, MOLLIS, DURUS, DURUS, 
        MOLLIS, MOLLIS, DURUS, DURUS,
        MOLLIS, DURUS, DURUS, MOLLIS
    };
    int system = mode_system[mode];
    char *key[] = { "c\\major", "f\\major" };
        
    for (i = 0; i < MAX_VOICE; ++i) {
        v_name = voice_name[i];
        fprintf(outfile,
                "      \\new Staff\n"
                "      <<\n"
                "        \\new Voice = \"%s\" {\n"
                "           \\clef \"%s\"\n"
                "              \\time %s\n"
                "              \\key %s\n"
                "              \\Music%s\n"
                "        }\n"
                "        \\new Lyrics \\lyricsto \"%s\" { \\Lyrics }\n"
                "      >>\n",
                v_name,
                clef_name[i],
                ly_meter[meter],
                key[system],
                v_name,
                v_name);
    }
    return;
}

void print_score(FILE *outfile, int mode, int meter) {
    fprintf(outfile, 
        "\\score {\n"
        "  <<\n"
        "    \\new ChoirStaff\n"
        "    <<\n");
    print_voice_commands(outfile, mode, meter);
    fprintf(outfile, 
        "    >>\n"
        "  >>\n"
        "}\n");
    return;
}

void print_voices(FILE *outfile, chorus_ptr chorus) {
    int i;
    for (i = 0; i < MAX_VOICE; ++i) {
        fprintf(outfile, "Music%s = {", voice_name[i]);
        list_print_music(outfile, select_voice(chorus, i));
        fprintf(outfile, " \\bar \"|.\" }\n\n");
    }
    return;
}

void print_music(FILE *outfile, node_ptr text, 
        chorus_ptr music, int mode, int meter) {
    print_version(outfile, LY_VERSION);
    print_lyrics(outfile, text);
    print_voices(outfile, music);
    print_score(outfile, mode, meter);
    return;
}

void chorus_free(chorus_ptr chorus) {
    int i;
    for (i = 0; i < MAX_VOICE; ++i) {
        music_list_free(select_voice(chorus, i));
    }
    return;
}

void music_list_free(music_node_ptr ls) {
    if (ls == NULL) {
        free(ls);
    } else {
        music_list_free(ls->next);
    }
    return;
}





