/* scribo.c 
 * Arca module to take pre-scanned text and output music in Lilypond format 
 * Andrew A. Cashner
 * 2019/02/13
 */

#include "scribo.h"
#include "interval.h"

/* TODO consolidate with interval, update headers */
/* TODO print rests mei or ly */

void check_member(int var, int *values, int values_len) {
    /* Check if var is in list (array) of acceptable values */
    int i, result;
    bool found = false;

    for (i = 0; i < values_len; ++i) {
        if (var == values[i]) {
            break;
            found = true;
        }
    }
    assert(found == true);
    return;
}
void check_range(int var, int min, int max) {
    /* Check var for min and max allowable values (<=, >=) */
    assert(var >= min && var <= max);
    return;
}
void check_voice_range(int voice_num) {
    range_check(voice_num, 0, 3);
    return;
}
void check_ptr(void *ptr) {
    assert(ptr != NULL);
}

char voice_name(int voice_num) {
    char voice_letter[] = "SATB";
    check_range(voice_num);
    return(voice_name[voice_num]);
}
char *clef_name(int voice_num) {
    char *clef_str[] = { "treble", "treble", "treble_8", "bass" };
    check_voice_range(voice_num);
    return(clef_str[voice_num]);
}
char *ly_meter(int meter_code) {
    char *meter_str[] = { "4/2", "3/1", "3/2" };
    int meter_code[] = { 2, 3, 32 };
    check_member(meter_code, &meter_code[0], 3);
    return(meter_str[meter_code]);
}

typedef struct chorus {
    note_ls_ptr music[MAX_VOICE];
}
typedef chorus *chorus_ptr;

chorus_ptr chorus_create(chorus_ptr choir) {
    int i;
    choir = malloc(sizeof(chorus));
    for (i = 0; i < MAX_VOICE; ++i) {
        choir->music[i] = note_ls_create();
    }
    return(choir);
}

note_ls_ptr select_voice(chorus_ptr chorus, int voice) {
    check_ptr(chorus);
    check_voice_range(voice);
    return(chorus->music[voice]);
}

void chorus_free(chorus_ptr choir) {
    for (i = 0; i < MAX_VOICE; ++i) {
        free(choir->music[i]);
    }
    free(choir);
    return;
}

chorus_ptr music_create(chorus_ptr chorus, node_ptr lyrics_ls, 
        syntagma_ptr syntagma, int mode, int meter) {

    int i, syllables, penult_len;
    int test, vperm_index, rperm_index;
    pinax_ptr pinax = NULL;
    col_ptr col = NULL;
    note_ls_ptr voice_part = NULL;
    node_ptr curr_lyrics = NULL;
    char error_msg[MAX_LINE];
    int seed = time(NULL);
    srand(seed);

    check_ptr(chorus);
    check_ptr(lyrics_ls);
    check_ptr(syntagma);

    for (curr_lyrics = lyrics_ls; curr_lyrics != NULL; 
            curr_lyrics = curr_lyrics->next) {

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

        chorus = compose(chorus, col, mode, vperm_index, meter, rperm_index);
    }
    return(chorus);
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

int pitch_class(char c) {
    char name[] = "cdefgab";
    int i;
    assert(c >= 'a' && c <= 'g');

    for (i = 0; i < 7; ++i) {
        if (name[i] == c) {
            break;
        }
    }
    return(i);
}

char pitch_name(int pitch_class) {
    char name[] = "cdefgab";
    assert(pitch_class >= 0 && pitch_class <= 6);
    return(name[pitch_class]);
}

char accid_name_mei(int accid_code) {
    int offset = 1;
    char *accid_name = "fns";

    assert(accid_code >= -1 && accid_code <= 1);

    if (accid_code == 0) {
        offset = 4; /* Return '\0' */
    }
    return(accid_name[offset + accid_code]);
}

char *accid_name_ly(int accid_code) {
    char *accid_str[] = { "es", "", "is" };
    int offset = 1;
    return(accid_str[offset + accid_code]);
}

char *octave_ticks_ly(int oct) {
    char *octave_ticks[] = {
        ",,,",
        ",,",
        ",",
        "",
        "\'",
        "\'\'",
        "\'\'\'",
    };
    assert(oct >= 0 && oct <= 6);
    return(octave_ticks[oct]);
}

char *dur_mei(int dur) {
    char *dur_name[] = {
        "breve' dots='1", "breve",
        "1' dots='1", "1",
        "2' dots='1", "2",
        "4' dots='1", "4",
        "8' dots='1", "8"
    };
    return(dur_name[dur]);
}
    
char *dur_ly(int dur) {
    char *dur_name[] = {
        "\\breve.", "\\breve",
        "1.", "1",
        "2.", "2",
        "4.", "4",
        "8.", "8"
    };
    return(dur_name[dur]);
}


void note_to_mei(note_ptr note) {
    assert(note != NULL);
    if (note->accid == 0) { /* natural */
        printf("<note pname='%c' oct='%d' dur='%s'></note>\n",
                pitch_name(note->pnum), 
                note->oct, 
                dur_mei(note->dur));
    } else {
        printf("<note pname='%c' oct='%d' accid='%c' dur='%s'></note>\n",
                pitch_name(note->pnum), 
                note->oct,
                accid_name_mei(note->accid),
                dur_mei(note->dur));
    }
    return;
}

void note_to_ly(note_ptr note) {
    assert(note != NULL);
    printf("%c%s%s%s ", 
            pitch_name(note->pnum),
            accid_name_ly(note->accid),
            octave_ticks_ly(note->oct),
            dur_ly(note->dur));
    return;
}




