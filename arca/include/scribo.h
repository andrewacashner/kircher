/* scribo.h */
#ifndef SCRIBO_H
#define SCRIBO_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lectio.h"
#include "arca.h"

#define LY_VERSION "2.19"
#define MAX_MUSIC 256

typedef enum VOICE_NAMES {
    CANTUS, ALTUS, TENOR, BASSUS
} VOICE_NAMES;
extern enum VOICE_NAMES voice_names;

typedef struct music_node *music_node_ptr;
typedef struct music_node {
    music_node_ptr next;
    char text[MAX_MUSIC];
} music_node;

music_node_ptr music_node_create(void);
music_node_ptr music_node_set(music_node_ptr node, 
        music_node_ptr next, char *text);
music_node_ptr last_music(music_node_ptr ls);
music_node_ptr music_ls_append(music_node_ptr ls, music_node_ptr node);
music_node_ptr *music_list(music_node_ptr *music, node_ptr lyrics_ls, 
        syntagma_ptr syntagma, int mode, int meter);
music_node_ptr compose(music_node_ptr music_ls, int voice_num,
        col_ptr col, int mode, int vperm_index, 
        int rperm_type, int rperm_index);
void list_print_text(FILE *outfile, node_ptr ls);
void print_lyrics(FILE *outfile, node_ptr ls);
void print_version(FILE *outfile, char *v_num);
void print_voice_commands(FILE *outfile, int meter);
void print_score(FILE *outfile, int meter);
void print_voices(FILE *outfile, music_node_ptr *chorus, int voice);
void print_music(FILE *outfile, node_ptr text, music_node_ptr *music, int meter);

#endif

