/* scribo.c 
 * Arca module to take pre-scanned text and output music in Lilypond format 
 * Andrew A. Cashner
 * 2019/02/13
 */

#include "scribo.h"

/* toni = church keys? */
/* 
 * int mode[MAX_MODE][MAX_SCALE] = {
    { nD, nE, nF, nG, nA, nBf, nCs, nD },
    { nG, nA, nBf, nC, nD, nEf, nFs, nG },
    { nA, nB, nC, nD, nE, nF, nGs, nA },
    { nA, nB, nCs, nD, nE, nF, nG, nA },
    { nBf, nC, nD, nE, nF, nG, nA, nBf },
    { nF, nG, nA, nBf, nC, nD, nE, nF },
    { nG, nA, nB, nC, nD, nE, nFs, nG },
    { nG, nA, nB, nC, nD, nE, nFs, nG },
    { nD, nE, nF, nG, nA, nBf, nCs, nD },
    { nA, nBf, nCs, nD, nE, nF, nG, nA },
    { nC, nD, nE, nF, nG, nA, nB, nC },
    { nF, nG, nA, nBf, nC, nD, nE, nF }
};
*/
/* TODO print rests mei or ly */

char voice_name(int voice_num) {
    char voice_letter[] = "SATB";
    assert(voice_num >= 0); assert(voice_num <= MAX_VOICE);
    return(voice_letter[voice_num]);
}
char *clef_name(int voice_num) {
    char *clef_str[] = { "treble", "treble", "treble_8", "bass" };
    assert(voice_num >= 0); assert(voice_num <= MAX_VOICE);
    return(clef_str[voice_num]);
}
char *ly_meter(int meter) {
    char *meter_str[] = { "4/2", "3/1", "3/2" };
    return(meter_str[meter]);
}
char *key(int mode) {
    int mode_system_dur_mol[] = {
        DURUS, MOLLIS, DURUS, DURUS, 
        MOLLIS, MOLLIS, DURUS, DURUS,
        MOLLIS, DURUS, DURUS, MOLLIS
    };
    /* TODO consolidate with other dur/mol test */
    char *key_str[] = { "c\\major", "f\\major" };

    int system = mode_system_dur_mol[mode];
    return(key_str[system]);
}

void textlist_print(FILE *outfile, textlist_ptr ls) {
    if (ls != NULL) {
        fprintf(outfile, "%s\n", ls->text);
        textlist_print(outfile, ls->next);
    }
    return;
}

void print_lyrics(FILE *outfile, textlist_ptr ls) {
    fprintf(outfile, "\nLyrics = \\lyricmode {\n  ");
    textlist_print(outfile, ls);
    fprintf(outfile, "}\n\n");
    return;
}

void print_version(FILE *outfile, char *v_num) {
    fprintf(outfile, "\\version \"%s\"\n", v_num);
    return;
}


void print_voices(FILE *outfile, chorus_ptr chorus) {
    int i;
    assert(chorus != NULL);
    for (i = 0; i < MAX_VOICE; ++i) {
        fprintf(outfile, "Music%c = { ", voice_name(i));
        notelist_to_ly(outfile, select_voice(chorus, i));
        fprintf(outfile, " \\bar \"|.\" }\n\n");
    }
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

void print_music(FILE *outfile, textlist_ptr text, 
        chorus_ptr music, int mode, int meter) {
    print_version(outfile, LY_VERSION);
    print_lyrics(outfile, text);
    print_voices(outfile, music);
    print_score(outfile, mode, meter);
    return;
}

int pitch_class(char c) {
    char pitch_letter[] = "cdefgab";
    int i;
    assert(c >= 'a' && c <= 'g');

    for (i = 0; i < 7; ++i) {
        if (pitch_letter[i] == c) {
            break;
        }
    }
    return(i);
}

char pitch_name(int pitch_class) {
    char pitch_letter[] = "cdefgab";
    assert(pitch_class >= 0); 
    assert(pitch_class <= 6);
    return(pitch_letter[pitch_class]);
}

char accid_name_mei(int accid_code) {
    char *accid_letter_mei = "fns";
    int offset = 1;
    assert(accid_code >= -1); assert(accid_code <= 1);

    if (accid_code == 0) {
        offset = 4; /* Return '\0' */
    }
    return(accid_letter_mei[offset + accid_code]);
}

char *accid_name_ly(int accid_code) {
    char *accid_str_ly[] = { "es", "", "is" };
    int offset = 1;
    assert(accid_code >= -1);
    assert(accid_code <= 1);
    return(accid_str_ly[offset + accid_code]);
}

char *octave_ticks_ly(int oct) {
    char *octave_tick_str[] = {
        ",,,",
        ",,",
        ",",
        "",
        "\'",
        "\'\'",
        "\'\'\'",
        "\'\'\'\'"
    };
    if (oct < 0) { 
        fprintf(stderr, "Octave %d too low!\n", oct); 
        return("LOW");
    } else if (oct > 7) { 
        fprintf(stderr, "Octave %d too high!\n", oct); 
        return("HIGH");
    } else {
        return(octave_tick_str[oct]);
    }
}
char *dur_mei(int dur) {
    char *dur_name_mei[] = {
        "breve' dots='1", "breve",
        "1' dots='1", "1",
        "2' dots='1", "2",
        "4' dots='1", "4",
        "8' dots='1", "8"
    };
   return(dur_name_mei[dur]);
}
char *dur_ly(int dur) {
    char *rhythm_names[] = {
        " ",
        "\\breve. ",
        "\\breve ",
        "1. ",
        "1 ",
        "2. ",
        "2 ",
        "4. ",
        "4 ",
        "8 ",
        "r\\breve ",
        "r1 ",
        "r2 ",
        "r4 ",
        "ERROR"
    };
    return(rhythm_names[dur]);
}

void chorus_to_mei(FILE *outfile, chorus_ptr choir) {
    int i;
    fprintf(outfile, "<mei>\n<score>\n");
    for (i = 0; i < MAX_VOICE; ++i) {
        debug_print("chorus_to_mei", "voice", i);
        fprintf(outfile, "<layer id='%c'>\n", "SATB"[i]);
        notelist_to_mei(outfile, select_voice(choir, i));
        fprintf(outfile, "</layer>\n");
    }
    fprintf(outfile, "</score>\n</mei>\n");
    return;
}


void notelist_to_mei(FILE *outfile, note_ptr ls) {
    if (ls != NULL) {
        debug_print("notelist_to_mei", "&ls", (long int)ls);
        note_to_mei(outfile, ls);
        notelist_to_mei(outfile, ls->next);
    }
    return;
}

void note_to_mei(FILE *outfile, note_ptr note) {
    assert(note != NULL);
    debug_print("note_to_mei", "&note", (long int)note);
    if (note->pnum == REST) {
        fprintf(outfile, "<rest dur='%s'></rest>\n", dur_mei(note->dur));
    } else if (note->accid == 0) { /* natural */
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

void notelist_to_ly(FILE *outfile, note_ptr ls) {
    if (ls != NULL) {
        debug_print("notelist_to_ly", "ls->pnum", ls->pnum);
        note_to_ly(outfile, ls);
        notelist_to_ly(outfile, ls->next);
    } else { 
        debug_print("notelist_to_ly", "received null list", 0); 
    }
    return;
}

void note_to_ly(FILE *outfile, note_ptr note) {
    assert(note != NULL);
    if (note->pnum == REST) {
        fprintf(outfile, "r%s ", dur_ly(note->dur));
        /* TODO bar rests */
    } else {
        fprintf(outfile, "%c%s%s%s ", 
                pitch_name(note->pnum),
                accid_name_ly(note->accid),
                octave_ticks_ly(note->oct),
                dur_ly(note->dur));
    }
    return;
}




