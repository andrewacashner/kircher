/* scribo.c 
 * Arca module to take pre-scanned text and output music in Lilypond format 
 * Andrew A. Cashner
 * 2019/02/13
 */

#include "scribo.h"

/* TODO print rests mei or ly */

char voice_name(int voice_num) {
    char voice_letter[] = "SATB";
    check_voice_range(voice_num);
    return(voice_letter[voice_num]);
}
char *clef_name(int voice_num) {
    char *clef_str[] = { "treble", "treble", "treble_8", "bass" };
    check_voice_range(voice_num);
    return(clef_str[voice_num]);
}
char *ly_meter(int meter) {
    char *meter_str[] = { "4/2", "3/1", "3/2" };
    return(meter_str[meter]);
}
/* TODO consolidate with other dur/mol test */
char *key(int mode) {
    int mode_system[] = {
        DURUS, MOLLIS, DURUS, DURUS, 
        MOLLIS, MOLLIS, DURUS, DURUS,
        MOLLIS, DURUS, DURUS, MOLLIS
    };
    int system = mode_system[mode];
    char *key_str[] = { "c\\major", "f\\major" };
    return(key_str[system]);
}

void list_print_text(FILE *outfile, textlist_ptr ls) {
    if (ls != NULL) {
        fprintf(outfile, "%s\n", ls->text);
        list_print_text(outfile, ls->next);
    }
    return;
}

void list_print_music(FILE *outfile, notelist_ptr ls) {
    if (ls != NULL) {
        note_to_ly(outfile, ls->note);
        list_print_music(outfile, ls->next);
    }
    return;
}

void print_lyrics(FILE *outfile, textlist_ptr ls) {
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
       
    for (i = 0; i < MAX_VOICE; ++i) {
        fprintf(outfile,
                "      \\new Staff\n"
                "      <<\n"
                "        \\new Voice = \"%c\" {\n"
                "           \\clef \"%s\"\n"
                "              \\time %s\n"
                "              \\key %s\n"
                "              \\Music%c\n"
                "        }\n"
                "        \\new Lyrics \\lyricsto \"%c\" { \\Lyrics }\n"
                "      >>\n",
                voice_name(i),
                clef_name(i),
                ly_meter(meter),
                key(mode),
                voice_name(i),
                voice_name(i));
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
        fprintf(outfile, "Music%c = {", voice_name(i));
        list_print_music(outfile, select_voice(chorus, i));
        fprintf(outfile, " \\bar \"|.\" }\n\n");
    }
    return;
}

void print_music(FILE *outfile, textlist_ptr text, 
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
    check_range(pitch_class, 0, 6);
    return(name[pitch_class]);
}

char accid_name_mei(int accid_code) {
    int offset = 1;
    char *accid_name = "fns";
    check_range(accid_code, -1, 1);

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
    check_range(oct, 0, 6);
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

void chorus_to_mei(FILE *outfile, chorus_ptr choir) {
    int i;
    fprintf(outfile, "<mei>\n<score>\n");
    for (i = 0; i < MAX_VOICE; ++i) {
        fprintf(outfile, "<layer id='%c'>\n", "SATB"[i]);
        notelist_to_mei(outfile, choir->music[i]);
        fprintf(outfile, "</layer>\n");
    }
    fprintf(outfile, "</score>\n</mei>\n");
    return;
}

#define DEBUG_PRINT(F,V) printf("DEBUG %s: %s\n", F, V);

void notelist_to_mei(FILE *outfile, notelist_ptr ls) {
    if (ls != NULL) {
        DEBUG_PRINT(notelist_to_mei, ls->note);
        note_to_mei(outfile, ls->note);
        notelist_to_mei(outfile, ls->next);
    }
    return;
}

void note_to_mei(FILE *outfile, note_ptr note) {
    check_ptr(note);
    if (note->accid == 0) { /* natural */
        fprintf(outfile, 
                "<note pname='%c' oct='%d' dur='%s'></note>\n",
                pitch_name(note->pnum), 
                note->oct, 
                dur_mei(note->dur));
    } else {
        fprintf(outfile,
                "<note pname='%c' oct='%d' accid='%c' dur='%s'></note>\n",
                pitch_name(note->pnum), 
                note->oct,
                accid_name_mei(note->accid),
                dur_mei(note->dur));
    }
    return;
}

void note_to_ly(FILE *outfile, note_ptr note) {
    check_ptr(note);
    fprintf(outfile, "%c%s%s%s ", 
            pitch_name(note->pnum),
            accid_name_ly(note->accid),
            octave_ticks_ly(note->oct),
            dur_ly(note->dur));
    return;
}




