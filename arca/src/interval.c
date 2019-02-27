/* interval.c 
 * Andrew A. Cashner
 * 2019/02/27
 *
 * Structures and functions for calculating, adjusting musical pitches and
 * intervals 
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

enum {
    pcC, pcD, pcE, pcF, pcG, pcA, pcB
} pitch_class_code;

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

typedef struct note {
    int pnum;  /* Pitch class 0-6 */
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
note_ptr note_set(note_ptr note, int pnum, int oct, int accid, int dur);
note_ptr note_normalize(note_ptr note);
note_ptr note_oct_shift(note_ptr note, int dir);
note_ptr note_oct_lower(note_ptr note);

int std_pnum(int oct, int pnum);
int note_to_std_pnum(note_ptr note);
int note_arithmetic(int (*fn)(int a, int b), note_ptr n1, note_ptr n2);
int add(int a, int b);
int subtract(int a, int b);
int note_sum(note_ptr n1, note_ptr n2);
int note_diff(note_ptr n1, note_ptr n2);
int note_cmp(note_ptr n1, note_ptr n2);

note_ls_ptr note_ls_create(void);
note_ls_ptr note_ls_set(note_ls_ptr ls, note_ptr note);
note_ls_ptr note_ls_last(note_ls_ptr ls);
note_ls_ptr note_ls_append(note_ls_ptr ls, note_ls_ptr new);
note_ls_ptr note_ls_set_append(note_ls_ptr ls, int pnum, int oct, int accid, int dur);
void note_ls_free(note_ls_ptr ls);
void note_ls_apply(void (*fn)(note_ptr note), note_ls_ptr music);
note_ls_ptr note_ls_map(note_ptr (*fn)(note_ptr note), note_ls_ptr music);
note_ls_ptr note_ls_map_inner(note_ptr (*fn)(note_ptr note), note_ls_ptr music);

note_ls_ptr arca_to_note_ls(int voice, int syl, int *pitch_num, 
        int *rhythm_num, int mode);
int pnum_in_mode(int pnum, int mode);


note_ls_ptr note_ls_adj_accid(note_ls_ptr music, int mode);
note_ptr note_accid_set(note_ptr note, int accid);
note_ptr note_accid_test_set(note_ptr note, int pnum, int accid);
note_ptr b_flat(note_ptr note);
note_ptr c_sharp(note_ptr note);
note_ptr ficta(note_ptr n1, note_ptr n2);
note_ls_ptr note_ls_adj_oct(note_ls_ptr music, int voice);
note_ls_ptr note_ls_adj_interval(note_ls_ptr music);

/* TODO convert this to "compose" function with top variables as parameters,
 * receiving input from arca */
int main(void) {
    int voice;
    int syl = 4;
    int notes[][4] = {
        { 7, 6, 6, 7 },
        { 4, 4, 4, 4 },
        { 2, 1, 1, 2 },
        { 7, 4, 4, 0 }
    };
    int rhythms[] = { MN, SM, SM, SB };
    int mode = 3;

    note_ls_ptr music[4];

    for (voice = 0; voice < 4; ++voice) {
        music[voice] = arca_to_note_ls(voice, syl, &notes[voice][0], &rhythms[0], mode);
        music[voice] = note_ls_adj_oct(music[voice], voice);
        music[voice] = note_ls_adj_accid(music[voice], mode);
        music[voice] = note_ls_adj_interval(music[voice]);
    }

    for (voice = 0; voice < 4; ++voice) {
        note_ls_apply(note_to_mei, music[voice]);
    }
    for (voice = 0; voice < 4; ++voice) {
        note_ls_apply(note_to_ly, music[voice]); 
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
    int octave_max[] = { 5, 4, 4, 3 };
    int pnum;
    int oct = octave_max[voice];
    int accid = NA;

    for (i = 0; i < syl; ++i) {
        pnum = pnum_in_mode(pitch_num[i], mode);
        music = note_ls_set_append(music, pnum, oct, accid, rhythm_num[i]);
    }
    return(music);
}

int pnum_in_mode(int pnum, int mode) {
    int mode_offset[] = { 
        pcD, pcG, pcA, 
        pcA, pcB, pcF, 
        pcG, pcG, pcD, 
        pcA, pcC, pcF,
    };
    assert(mode >= 0 && mode < 12);
    return(pnum + mode_offset[mode]);
}

note_ls_ptr note_ls_adj_oct(note_ls_ptr music, int voice) {
    note_ls_ptr curr = NULL;
    int test;
    bool too_high_tf = false;
    note range_max[] = { 
        { pcG, 5, 0, 0 },
        { pcD, 5, 0, 0 },
        { pcF, 4, 0, 0 },
        { pcC, 4, 0, 0 }
    };

    assert(music != NULL);
    assert(voice >= 0 && voice < 4);

    for (curr = music; curr != NULL; curr = curr->next) {
        test = note_cmp(curr->note, &range_max[voice]);
        if (test > 0) {
            too_high_tf = true;
            break;
        }
    }
    if (too_high_tf == true) {
        music = note_ls_map(note_oct_lower, music);
    }
    return(music);
}
/* TODO check minimums also */
/* TODO check distance between voice also (& swap voices) */

note_ls_ptr note_ls_adj_accid(note_ls_ptr music, int mode) {
    note_ptr n1 = NULL;
    note_ptr n2 = NULL;
    note_ls_ptr curr = NULL;

    bool mode_mollis_tf[] = {
        false, true, false, 
        false, true, true, 
        false, false, true, 
        false, false, true
    };
    bool mode_ficta_tf[] = {
        true, true, true, 
        false, false, false,
        true, true, true,
        false, false, false
    };
    int mode_sharp3 = 3;

    for (curr = music; curr->next != NULL; curr = curr->next) {
        n1 = curr->note;
        n2 = curr->next->note;

        if (mode_mollis_tf[mode] == true) {
            n1 = b_flat(n1);
            n2 = b_flat(n2);
        }
        if (mode == mode_sharp3) {
            n1 = c_sharp(n1);
            n2 = c_sharp(n2);
        }
        if (mode_ficta_tf[mode] == true) {
            n1 = ficta(n1, n2);
        }
    }
    return(music);
}

note_ptr note_accid_set(note_ptr note, int accid) {
    assert(note != NULL);
    assert(accid >= FL && accid <= SH);
    note->accid = accid;
    return(note);
}
note_ptr note_accid_test_set(note_ptr note, int pnum, int accid) {
    if (note->pnum == pnum) {
        note = note_accid_set(note, accid);
    }
    return(note);
}   
note_ptr b_flat(note_ptr note) {
    return(note_accid_test_set(note, pcB, FL));
}
note_ptr c_sharp(note_ptr note) {
    return(note_accid_test_set(note, pcC, SH));
}
note_ptr ficta(note_ptr n1, note_ptr n2) {
    int pnum1 = n1->pnum;
    int pnum2 = n2->pnum;
    int accid = n1->accid;

    if ((pnum1 == pcB && pnum2 == pcA) 
            || (pnum1 == pcE && pnum2 == pcD)) {
        accid = FL;
    } else if ((pnum1 == pcC && pnum2 == pcD) 
            || (pnum1 == pcF && pnum2 == pcG)) {
        accid = SH;
    }
    n1 = note_accid_set(n1, accid);
    return(n1);
}

note_ls_ptr note_ls_adj_interval(note_ls_ptr music) {
    note_ls_ptr curr = NULL;
    note_ptr n1, n2;
    int test;
    for (curr = music; curr->next != NULL; curr = curr->next) {
        n1 = curr->note;
        n2 = curr->next->note;
        test = note_diff(n1, n2);
        if (test == 0) {
            /* If two repeated pnums have diff accidentals, use the 
             * second one
             */
            if (n1->accid != n2->accid) {
                n1->accid = n2->accid;
            }
        } else if (test > 5) {
        /* If the first note is too far above the second, lower the first;
         * if the first note is too far below the second, lower the second
         */
            n1 = note_oct_shift(n1, -1);
        } else if (test < -5) {
            n2 = note_oct_shift(n2, -1);
        }
    }
    return(music);
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



note_ptr note_create(void) {
    note_ptr note = malloc(sizeof(note));
    return(note);
}

note_ptr note_set(note_ptr note, int pnum, int oct, int accid, int dur) {
    assert(note != NULL);
    note->pnum = pnum;
    note->oct = oct;
    note->accid = accid;
    note->dur = dur;

    note = note_normalize(note);
    return(note);
}


note_ptr note_normalize(note_ptr note) {
    assert(note != NULL);

    if (note->pnum > 7) {
        note->oct += note->pnum / 7;
    } else if (note->pnum < 0) {
        note->oct -= note->pnum / 7;
    }

    note->pnum %= 7;

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

note_ptr note_oct_shift(note_ptr note, int dir) {
    assert(note != NULL);
    assert(dir == 1 || dir == -1);
    note->oct += 1 * dir;
    return(note);
}

note_ptr note_oct_lower(note_ptr note) {
    return(note_oct_shift(note, -1));
}

int std_pnum(int oct, int pnum) {
    return(oct * 7 + pnum);
}
int note_to_std_pnum(note_ptr note) {
    assert(note != NULL);
    return(std_pnum(note->oct, note->pnum));
}

int note_arithmetic(int (*fn)(int a, int b), note_ptr n1, note_ptr n2) {
    int n1_std, n2_std;
    assert(n1 != NULL && n2 != NULL);
    n1_std = note_to_std_pnum(n1);
    n2_std = note_to_std_pnum(n2);
    return(fn(n1_std, n2_std));
}
int add(int a, int b) {
    return(a + b);
}
int subtract(int a, int b) {
    return(a - b);
}
int note_sum(note_ptr n1, note_ptr n2) {
    return(note_arithmetic(add, n1, n2));
}
int note_diff(note_ptr n1, note_ptr n2) {
    return(note_arithmetic(subtract, n1, n2));
}

int note_cmp(note_ptr n1, note_ptr n2) {
    int result = 0;
    int test;
    assert(n1 != NULL && n2 != NULL);

    test = note_diff(n1, n2);
    if (test == 0) {
        result = 0;
    } else if (test > 0) {
        result = 1;
    } else if (test < 0) {
        result = -1;
    }
    return(result);
}

note_ls_ptr note_ls_create(void) {
    note_ls_ptr ls = malloc(sizeof(note_ls));
    ls->note = malloc(sizeof(note));
    return(ls);
}

note_ls_ptr note_ls_set(note_ls_ptr ls, note_ptr note) {
    assert(ls != NULL && note != NULL);
    ls->note = note_set(ls->note, 
            note->pnum, note->oct, note->accid, note->dur);
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

note_ls_ptr note_ls_set_append(note_ls_ptr ls, int pnum, int oct, int accid, int dur) {
    note_ptr note = note_create();
    note_ls_ptr music = note_ls_create();

    note = note_set(note, pnum, oct, accid, dur);
    music = note_ls_set(music, note);

    ls = note_ls_append(ls, music);
    return(ls);
}

void note_ls_apply(void (*fn)(note_ptr note), note_ls_ptr music) {
    if (music != NULL) {
        fn(music->note);
        note_ls_apply(fn, music->next);
    }
    return;
}
note_ls_ptr note_ls_map(note_ptr (*fn)(note_ptr note), note_ls_ptr music) {
    note_ls_ptr head = music;
    music = note_ls_map_inner(fn, music);
    return(head);
}
note_ls_ptr note_ls_map_inner(note_ptr (*fn)(note_ptr note), note_ls_ptr music) {
    if (music != NULL) {
        music->note = fn(music->note);
        note_ls_map_inner(fn, music->next);
    }
    return(music);
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







