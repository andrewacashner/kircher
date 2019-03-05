/* scribo.h */
#ifndef SCRIBO_H
#define SCRIBO_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "arca.h"
#include "lectio.h"
#include "cogito.h"

#define END_STR_ARRAY "\0"

/* FUNCTION PROTOTYPES */
/* To extract data from notes for printing */
int pitch_class(char c);
char pitch_name(int pitch_class);
char accid_name_mei(int accid_code);
char *accid_name_ly(int accid_code, int accid_type);
char *octave_ticks_ly(int oct);
char *dur_mei(int dur);
char *dur_ly(int dur);
/* To extract other date for printing */
char voice_name(int voice_num);
char *clef_name(int voice_num);
char *ly_meter(int meter_code);
char *key(int mode);

/* To print to different formats */
void chorus_to_mei(FILE *outfile, chorus_ptr choir);
void notelist_to_mei(FILE *outfile, note_ptr ls);
void note_to_mei(FILE *outfile, note_ptr note);
void notelist_to_ly(FILE *outfile, note_ptr ls);
void note_to_ly(FILE *outfile, note_ptr note);

void textlist_print(FILE *outfile, textlist_ptr ls);
void print_lyrics(FILE *outfile, textlist_ptr ls);
void print_version(FILE *outfile, char *v_num);
void print_include(FILE *outfile, char **name_array);
void print_voice_commands(FILE *outfile, int mode, int meter);
void print_score(FILE *outfile, int mode, int meter);
void print_voices(FILE *outfile, chorus_ptr chorus);
void print_music(FILE *outfile, textlist_ptr text, 
        chorus_ptr music, int mode, int meter);

#endif
