#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

enum {
   FL = -1, NA = 0, SH = 1
} accid_code;

enum {
    BRD, BR, 
    SBD, SB,
    MND, MN,
    SMD, SM,
    FSD, FS
} dur_code;

enum {
    DURUS, MOLLIS
} mode_system_code;



typedef struct note {
    int pname;  /* Pitch class 0-6 */
    int oct;    /* Helmholtz octave */
    int accid;  /* enum accid_code */
    int dur;    /* enum dur_code */
} note;
typedef note *note_ptr;

typedef struct note_ls *note_ls_ptr;
typedef struct note_ls {
    note_ptr note;
    note_ls_ptr next;
} note_ls;


int pitch_class(char c);
char pitch_name(int pitch_class);
char accid_name_mei(int accid_code);
char *accid_name_ly(int accid_code);
char *octave_ticks_ly(int oct);
char *dur_mei(int dur);
char *dur_ly(int dur);
void note_to_mei(note_ptr note);
void note_to_ly(note_ptr note);

note_ptr note_create(void);
note_ptr note_set(note_ptr note, int pname, int oct, int accid, int dur);
note_ptr note_normalize(note_ptr note);
note_ptr note_sum(note_ptr sum, note_ptr n1, note_ptr n2);

note_ls_ptr note_ls_create(void);
note_ls_ptr note_ls_set(note_ls_ptr ls, note_ptr note);
note_ls_ptr note_ls_last(note_ls_ptr ls);
note_ls_ptr note_ls_append(note_ls_ptr ls, note_ls_ptr new);
note_ls_ptr note_ls_set_append(note_ls_ptr ls, int pname, int oct, int accid, int dur);
void note_ls_free(note_ls_ptr ls);

void print_music(note_ls_ptr music, void (*fn)(note_ptr note));

note_ls_ptr arca_to_note_ls(int voice, int syl, int *pitch_num, 
        int *rhythm_num, int mode);
int pnum_in_mode(int pnum, int mode);




int main(void) {
    int voice;
    int syl = 2;
    int notes[] = { 6, 5 };
    int rhythms[] = { SB, SB };
    int mode = 1;
    note_ls_ptr music[4];

    for (voice = 0; voice < 4; ++voice) {
        music[voice] = arca_to_note_ls(voice, syl, &notes[0], &rhythms[0], mode);
    }

    for (voice = 0; voice < 4; ++voice) {
        print_music(music[voice], note_to_mei);
    }
    for (voice = 0; voice < 4; ++voice) {
        print_music(music[voice], note_to_ly); 
        puts("");
    }

    for (voice = 0; voice < 4; ++voice) {
        note_ls_free(music[voice]);
    }
    return(0);
}

note_ls_ptr arca_to_note_ls(int voice, int syl, int *pitch_num, 
        int *rhythm_num, int mode) {

    note_ls_ptr music = NULL;
    int i;
    int octave_max[4] = { 5, 4, 4, 3 };
    int mode_system[] = { DURUS, MOLLIS };
    int pnum, oct, accid;

    oct = octave_max[voice];

    for (i = 0; i < syl; ++i) {
        if (pitch_num[i] == 6 && mode_system[mode] == MOLLIS) {
            accid = FL;
        } else {
            accid = NA;
        }
        pnum = pnum_in_mode(pitch_num[i], mode);
        music = note_ls_set_append(music, pnum, oct, accid, rhythm_num[i]);
    }
    return(music);
}

/* TODO separate functions to adjust registers and intervals */

int pnum_in_mode(int pnum, int mode) {
    /* TODO mode conversion */
    return(pnum);
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
        "\'\'",
        "\'\'\'",
    };
    assert(oct >= 0 && oct <= 7);
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
                pitch_name(note->pname), 
                note->oct, 
                dur_mei(note->dur));
    } else {
        printf("<note pname='%c' oct='%d' accid='%c' dur='%s'></note>\n",
                pitch_name(note->pname), 
                note->oct,
                accid_name_mei(note->accid),
                dur_mei(note->dur));
    }
    return;
}

void note_to_ly(note_ptr note) {
    assert(note != NULL);
    printf("%c%s%s%s ", 
            pitch_name(note->pname),
            accid_name_ly(note->accid),
            octave_ticks_ly(note->oct),
            dur_ly(note->dur));
    return;
}



note_ptr note_create(void) {
    note_ptr note = malloc(sizeof(note));
    return(note);
}

note_ptr note_set(note_ptr note, int pname, int oct, int accid, int dur) {
    assert(note != NULL);
    note->pname = pname;
    note->oct = oct;
    note->accid = accid;
    note->dur = dur;

    note = note_normalize(note);
    return(note);
}


note_ptr note_normalize(note_ptr note) {
    assert(note != NULL);

    if (note->pname > 7) {
        note->oct += note->pname / 7;
    } else if (note->pname < 0) {
        note->oct -= note->pname / 7;
    }

    note->pname %= 7;

    if (note->accid < FL) {
        note->accid = FL;
    } else if (note->accid > SH) {
        note->accid = SH;
    }

    if (note->dur < BRD) { /* XXX or error */
        note->dur = BRD;
    } else if (note->dur > FS) {
        note->dur = FS;
    }

    return(note);
}

note_ptr note_sum(note_ptr sum, note_ptr n1, note_ptr n2) {
    assert(sum != NULL && n1 != NULL && n2 != NULL);

    sum->pname = n1->pname + n2->pname;
    sum->oct = n1->oct + n2->oct;
    sum->accid = n1->accid + n2->accid;

    sum = note_normalize(sum);
    return(sum);
}


note_ls_ptr note_ls_create(void) {
    note_ls_ptr ls = malloc(sizeof(note_ls));
    ls->note = malloc(sizeof(note));
    return(ls);
}

note_ls_ptr note_ls_set(note_ls_ptr ls, note_ptr note) {
    assert(ls != NULL && note != NULL);
    ls->note = note_set(ls->note, 
            note->pname, note->oct, note->accid, note->dur);
    ls->next = NULL;
    return(ls);
}

note_ls_ptr note_ls_last(note_ls_ptr ls) {
    if (ls->next == NULL) {
        return(ls);
    } else {
        return(note_ls_last(ls->next));
    }
}

note_ls_ptr note_ls_append(note_ls_ptr ls, note_ls_ptr new) {
    note_ls_ptr head = ls;
    assert(new != NULL);
    if (ls == NULL) {
        head = new;
    } else {
        note_ls_last(ls)->next = new;
    }
    return(head);
}

note_ls_ptr note_ls_set_append(note_ls_ptr ls, int pname, int oct, int accid, int dur) {
    note_ptr note = note_create();
    note_ls_ptr music = note_ls_create();

    note = note_set(note, pname, oct, accid, dur);
    music = note_ls_set(music, note);

    ls = note_ls_append(ls, music);
    return(ls);
}

void print_music(note_ls_ptr music, void (*fn)(note_ptr note)) {
    if (music != NULL) {
        fn(music->note);
        print_music(music->next, fn);
    }
    return;
}


void note_ls_free(note_ls_ptr ls) {
    if (ls != NULL) {
        if (ls->next != NULL) {
            note_ls_free(ls->next);
        }
        free(ls->note);
        free(ls);
    }
    return;
}







