/* cogito.h */
#ifndef COGITO_H
#define COGITO_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <time.h>

#include "error.h"
#include "arca.h"

#define MAX_VOICE 4

/* ENUMS */
enum PITCH_CLASS_CODE {
    pcC, pcD, pcE, pcF, pcG, pcA, pcB, REST
};
extern enum PITCH_CLASS_CODE pitch_class_code;

enum ACCID_CODE {
   FL = -1, NA = 0, SH = 1
};
extern enum ACCID_CODE accid_code;


/* DATA STRUCTURES */
typedef struct note {
    int pnum;  /* Pitch class 0-6 */
    int oct;    /* Helmholtz octave */
    int accid;  /* enum accid_code */
    int dur;    /* enum dur_code */
} note;
typedef note *note_ptr;

typedef struct notelist *notelist_ptr;
typedef struct notelist {
    note_ptr note;
    notelist_ptr next;
} notelist;

typedef struct chorus {
    notelist_ptr music[MAX_VOICE];
} chorus;
typedef chorus *chorus_ptr;


/* FUNCTION PROTOTYPES */
/* To manipulate note structures */
note_ptr note_create(void);
note_ptr note_set(note_ptr note, int pnum, int oct, int accid, int dur);
note_ptr note_normalize(note_ptr note);
note_ptr note_oct_shift(note_ptr note, int dir);
note_ptr note_oct_lower(note_ptr note);

/* To do arithmetic on notes and compare them */
int std_pnum(int oct, int pnum);
int note_to_std_pnum(note_ptr note);
int note_arithmetic(int (*fn)(int a, int b), note_ptr n1, note_ptr n2);
int add(int a, int b);
int subtract(int a, int b);
int note_sum(note_ptr n1, note_ptr n2);
int note_diff(note_ptr n1, note_ptr n2);
int note_cmp(note_ptr n1, note_ptr n2);

/* To make a linked list of notes */
notelist_ptr notelist_create(void);
notelist_ptr notelist_set(notelist_ptr ls, note_ptr note);
notelist_ptr notelist_last(notelist_ptr ls);
notelist_ptr notelist_append(notelist_ptr ls, notelist_ptr new);
notelist_ptr notelist_set_append(notelist_ptr ls, int pnum, int oct, int accid, int dur);
notelist_ptr notelist_set_append_rest(notelist_ptr ls, int dur);
void notelist_free(notelist_ptr ls);

/* To operate on linked list */
void notelist_apply(void (*fn)(note_ptr note), notelist_ptr music);
notelist_ptr notelist_map(note_ptr (*fn)(note_ptr note), notelist_ptr music);
notelist_ptr notelist_map_inner(note_ptr (*fn)(note_ptr note), notelist_ptr music);

/* To make and manipulate an array of notelists for four voices */
chorus_ptr chorus_create(void);
notelist_ptr select_voice(chorus_ptr chorus, int voice);
void chorus_free(chorus_ptr choir);

/* To convert arca data to chorus array of notelists */
chorus_ptr chorus_compose(chorus_ptr chorus, textlist_ptr text, 
        syntagma_ptr syntagma, int mode, int meter);
notelist_ptr voice_compose(chorus_ptr chorus, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index);
notelist_ptr arca_to_notelist(notelist_ptr music, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index);
int pnum_in_mode(int pnum, int mode);

/* To adjust notes for compositional requirements */
notelist_ptr notelist_adj_accid(notelist_ptr music, int mode);
note_ptr note_accid_set(note_ptr note, int accid);
note_ptr note_accid_test_set(note_ptr note, int pnum, int accid);
note_ptr b_flat(note_ptr note);
note_ptr c_sharp(note_ptr note);
note_ptr ficta(note_ptr n1, note_ptr n2);
notelist_ptr notelist_adj_oct(notelist_ptr music, int voice);
notelist_ptr notelist_adj_interval(notelist_ptr music);

#endif
