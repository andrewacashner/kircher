/* cogito.h */
#ifndef COGITO_H
#define COGITO_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "error.h"
#include "arca.h"

#define MIN_PNUM 0
#define PNUM_BASE 7
#define MAX_PNUM PNUM_BASE - 1
#define DIR_UP 1
#define DIR_DOWN -1
#define MAX_INTERVAL 4
#define BLANK -99
#define MAX_VOICE_DISTANCE 8

/* ENUMS */
enum VOICE_INDEX {
    CANTUS, ALTUS, TENOR, BASSUS, MAX_VOICE
};
extern enum VOICE_INDEX voice_index;

enum NOTE_TYPE {
    PITCH, REST
};
extern enum NOTE_TYPE note_type;

enum PITCH_CLASS_CODE {
    pcC, pcD, pcE, pcF, pcG, pcA, pcB
};
extern enum PITCH_CLASS_CODE pitch_class_code;

enum ACCID_CODE {
   FL = -1, NA = 0, SH = 1
};
extern enum ACCID_CODE accid_code;

enum ACCID_TYPE {
    ACCID_DEFAULT,      /* Just do it */
    SIGNATURE,          /* Accidental matches key signature (= accid.ges in MEI) */
    FICTA,              /* Accidental is ficta recommendation */
    MAX_ACCID_TYPE
};
extern enum ACCID_TYPE accid_type;

/* DATA STRUCTURES */
typedef struct note *note_ptr;
typedef struct note {
    int type;       /* enum note_type */
    int pnum;       /* Pitch class 0-6 */
    int oct;        /* Helmholtz octave */
    int accid;      /* enum accid_code */
    int accid_type; /* enum accid_type */
    int dur;        /* enum dur_code */
    note_ptr next;
} note;

typedef struct chorus {
    note_ptr music[MAX_VOICE];
} chorus;
typedef chorus *chorus_ptr;


/* FUNCTION PROTOTYPES */
/* To manipulate note structures */
note_ptr note_create(void);
note_ptr note_set(note_ptr note, int pnum, int oct, int accid, int dur);
note_ptr rest_set(note_ptr note, int dur);

/* To make a linked list of notes */
note_ptr note_last(note_ptr ls);
note_ptr note_append(note_ptr ls, note_ptr new);
void note_free(note_ptr ls);
void note_apply(void (*fn)(note_ptr note), note_ptr music);
note_ptr note_map(note_ptr (*fn)(note_ptr note), note_ptr music);
note_ptr note_map_inner(note_ptr (*fn)(note_ptr note), note_ptr music);

/* To do arithmetic on notes and compare them */
note_ptr note_normalize(note_ptr note);
note_ptr note_oct_shift(note_ptr note, int dir);
note_ptr note_oct_lower(note_ptr note);
note_ptr note_oct_higher(note_ptr note);
int std_pnum(int oct, int pnum);
int note_to_std_pnum(note_ptr note);
int note_arithmetic(int (*fn)(int a, int b), note_ptr n1, note_ptr n2);
int add(int a, int b);
int subtract(int a, int b);
int note_sum(note_ptr n1, note_ptr n2);
int note_diff(note_ptr n1, note_ptr n2);
int note_cmp(note_ptr n1, note_ptr n2);

/* To make and manipulate an array of notelists for four voices */
chorus_ptr chorus_create(void);
note_ptr select_voice(chorus_ptr chorus, int voice);
void chorus_free(chorus_ptr choir);

/* To convert arca data to chorus array of notelists */
chorus_ptr chorus_compose(chorus_ptr chorus, textlist_ptr text, 
        syntagma_ptr syntagma, int mode, int meter);
note_ptr notelist_cat(note_ptr ls1, note_ptr ls2);
note_ptr voice_compose(note_ptr ls, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index);
note_ptr arca_to_notelist(note_ptr music, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index);
int pnum_in_mode(int pnum, int mode);
int mode_scale_deg(int scale_deg, int mode);

/* To adjust notes for compositional requirements */
note_ptr notelist_adj_oct(note_ptr music, int voice);
note_ptr notelist_adj_accid(note_ptr music, int mode);
note_ptr note_accid_set(note_ptr note, int accid, int accid_type);
note_ptr note_accid_cpy(note_ptr n1, note_ptr n2);
note_ptr note_accid_test_set(note_ptr note, int pnum, 
        int accid, int accid_type);
note_ptr note_accid_mode_test_set(note_ptr note, int pnum, int mode, 
        int accid, int accid_type);
note_ptr ficta(note_ptr n1, note_ptr n2, int mode);
note_ptr notelist_adj_interval(note_ptr music);
note_ptr notelist_adj_leaps(note_ptr music);

int notelist_ref(note_ptr ls, int index);
chorus_ptr voice_swap(chorus_ptr choir, int upper, int lower);
chorus_ptr chorus_adj_voice_distance(chorus_ptr choir);

#endif
