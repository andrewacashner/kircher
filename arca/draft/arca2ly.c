/* arca2ly.c
 * Convert output from Arca musarithmica to full Lilypond code 
 * Andrew A. Cashner, 2019/02/09
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* CONSTANTS, LABELS  */
#define MAX_CHAR 256*256
#define MAX_LINE 256
#define MAX_VOICE 4

char *voice_name[] = { "S", "A", "T", "B" };
char *clef_name[] = { "treble", "treble", "treble_8", "bass" };
char *rel_pitch[] = { "c\'\'", "c\'\'", "c\'", "c\'" };
char *ly_meter[] = { "4/2", "3/1", "3/2" };

enum {
    USAGE,
    NO_FILE_IN,
    NO_FILE_OUT,
    MAX_ERROR
} error_num;

char *error_str[] = {
    "Usage: arca2ly <infile> <outfile>",
    "Could not open input file for reading",
    "Could not open output file for writing"
};

enum { 
    READ_LYRICS, 
    READ_METER, 
    READ_MUSIC, 
    MAX_READ
} read_modes;

/* DATA STRUCTURES */
/* TODO replace with dynamic allocation */
typedef struct {
    char array[MAX_VOICE][MAX_CHAR];
} music_buf;
typedef music_buf *music_buf_ptr;


/* FUNCTION PROTOTYPES */
void exit_error(int error_code);
char *version(char *str, char *v_num);
char *voice_part(char *str, int voice, int meter);
void write_score(FILE *outfile, int meter, music_buf_ptr music, char *lyrics);

/* MAIN */
int main(int argc, char *argv[]) {
    FILE *infile, *outfile;
    char *infilename, *outfilename;
    char line[MAX_LINE];
    int meter_num, read_mode, voice_num;
    char lyrics[MAX_CHAR];
    music_buf music;
    music_buf_ptr music_ptr = &music;

    if (argc != 3) {
        exit_error(USAGE);
    }

    infilename = argv[1];
    infile = fopen(infilename, "r");
    if (infile == NULL) {
        exit_error(NO_FILE_IN);
    }

    outfilename = argv[2];
    outfile = fopen(outfilename, "w");
    if (outfile == NULL) {
        exit_error(NO_FILE_OUT);
    }

    read_mode = READ_LYRICS;
    voice_num = 0;

    while (read_mode < MAX_READ &&
            fgets(line, sizeof(line), infile) != NULL) {

        /* Double newline moves to next type to read: 
         * lyrics, meter_num, music */
        if (line[0] == '\n') {
            ++read_mode;
            continue;
        }

        switch (read_mode) {
            case READ_LYRICS:
                /* Copy into single string */
                strcat(lyrics, line);
                break;

            case READ_METER:
                sscanf(line, "%d", &meter_num);
                break;

            case READ_MUSIC:
                /* Copy each line (newline trimmed) into 
                 * one member of string array */
                line[strlen(line) - 1] = '\0';
                strcat(music_ptr->array[voice_num], line);
                ++voice_num;
                break;

            default:
                break;
        }
    }

    write_score(outfile, meter_num, music_ptr, lyrics);

    fclose(infile);
    fclose(outfile);

    return(0);
}

/* FUNCTIONS */
void exit_error(int error_code) {
    assert(error_code < MAX_ERROR);
    fprintf(stderr, "%s\n", error_str[error_code]);
    exit(EXIT_FAILURE);
}

char *version(char *str, char *v_num) {
    sprintf(str, "\\version \"%s\"\n", v_num);
    return(str);
}

void write_score(FILE *outfile, int meter, music_buf_ptr music, char *lyrics) {
    char tmp[MAX_CHAR];
    char *tmp_ptr = tmp;
    int i;
   
    fprintf(outfile, version(tmp_ptr, "2.19"));

    for (i = 0; i < MAX_VOICE; ++i) {
        fprintf(outfile, 
                "Music%s = {\n"
                "  %s\n"
                "}\n", voice_name[i], music->array[i]);
    }

    fprintf(outfile, 
            "\nLyrics = \\lyricmode {\n"
            "  %s\n"
            "}\n\n", lyrics);

    fprintf(outfile, 
        "\\score {\n"
        "  <<\n"
        "    \\new ChoirStaff\n"
        "    <<\n");
   
    for (i = 0 ; i < MAX_VOICE; ++i) {
        fprintf(outfile, "%s\n", voice_part(tmp_ptr, i, meter));
    }

    fprintf(outfile, 
        "    >>\n"
        "  >>\n"
        "}\n");
    return;
}

char *voice_part(char *str, int voice, int meter) {
    char *v_name = voice_name[voice];
    sprintf(str, 
            "      \\new Staff\n"
            "      <<\n"
            "        \\new Voice = \"%s\" {\n"
            "           \\clef \"%s\"\n"
            "           \\relative %s {\n"
            "              \\time %s\n"
            "              \\Music%s\n"
            "           }\n"
            "        }\n"
            "        \\new Lyrics \\lyricsto \"%s\" { \\Lyrics }\n"
            "      >>\n",
            v_name,
            clef_name[voice],
            rel_pitch[voice],
            ly_meter[meter],
            v_name,
            v_name);
    return(str);
}


