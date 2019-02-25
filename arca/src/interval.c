/* interval.c 
 * Module for calculating and adjusting pitches with octaves */

#include "interval.h"

int base_octave[] = { 5, 4, 4, 3 };
int max_range[] = { 38, 35, 32, 29 };
int mode_offset[] = {
    1, 4, 5, 5, 
    6, 3, 4, 4,
    1, 6, 0, 3
};

int std_pitch(int scale_degree, int mode_num, int octave) {
    return(scale_degree + mode_offset[mode_num] + octave * 7);
}

musarithm_ptr musarithm_create(void) {
    musarithm_ptr new = malloc(sizeof(musarithm));
    return(new);
}

musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index,
        int mode_num) {
    /* Copy vperm to musarithm structure and adjust too-large intervals */
    int voice, note; /* indices */
    int syl; 
    int pitch1, pitch2;
    int octave[MAX_VOICE];
    int max;

    assert(music != NULL && col != NULL && vperm_index < VPERM_Z);

    syl = col->syl;
    music->syl = syl;
    
    for (voice = 0; voice < MAX_VOICE; ++voice) {
        /* Find highest possible starting octave for each voice */
        max = max_range[voice];
        octave[voice] = base_octave[voice];
       
        /* Set octave: 
         * Check whole set of notes, if any are above the max, move the octave
         * down for all */
        for (note = 0; note < syl; ++note) { 
            pitch1 = std_pitch(get_pitch_num(col, vperm_index, voice, note), 
                    mode_num, octave[voice]);
            if (pitch1 > max) {
                --octave[voice];
                break;
            } 
        }
        /* Reduce intervals:
         * Check set again, now in pairs: Reduce too-large intervals and store
         * in musarithm array */
        for (note = 1; note < syl; ++note) {
            pitch1 = std_pitch(get_pitch_num(col, vperm_index, voice, note - 1), 
                    mode_num, octave[voice]);

            pitch2 = std_pitch(get_pitch_num(col, vperm_index, voice, note), 
                    mode_num, octave[voice]);
           
            /* If the interval is too great between the two notes,
             * shift the second one up or down depending on the direction */
            if (pitch2 - pitch1 > MAX_INTERVAL) {
                pitch2 -= 7;
            } else if (pitch1 - pitch2 > MAX_INTERVAL) {
                pitch1 -= 7;
            }

            /* Store adjusted pitches in musarithm array */
            music->array[voice][note - 1] = pitch1;
            music->array[voice][note] = pitch2;
        }
    }
    return(music);
}

char *std_pitch_to_ly(char *str, int std_pitch, int mode) {
    int pitch_class = std_pitch % 7;
    int octave = std_pitch / 7;
    char *scale = "cdefgab";
    char *flat = "es";
    char *ticks[] = {
        ",,",
        ",",
        "",
        "\'",
        "\'\'"
    };
    char pitch_str[10];
    char *octave_str;
    int mode_system[MAX_MODE] = {
        DURUS, MOLLIS, DURUS, DURUS, 
        MOLLIS, MOLLIS, DURUS, DURUS,
        MOLLIS, DURUS, DURUS, MOLLIS
    };

    assert(octave >= 2 && octave <= 5);
    pitch_str[0] = scale[pitch_class];
    pitch_str[1] = '\0';
    if (pitch_class == 6 && mode_system[mode] == MOLLIS ) {
        strcat(pitch_str, flat);
    }
    octave_str = ticks[octave - 1];
    sprintf(str, "%s%s", pitch_str, octave_str);

    return(str);
}

    
int mus_get_pitch(musarithm_ptr mus, int voice, int x) {
    return(mus->array[voice][x]);
}

int mus_get_pitch_class(musarithm_ptr mus, int voice, 
        int x, int mode_num) {
    int pc = mus->array[voice][x] - mode_offset[mode_num];
    return(pc % 7);
}

int mus_get_octave(musarithm_ptr mus, int voice, 
        int x, int mode_num) {
    int pc = mus->array[voice][x] - mode_offset[mode_num];
    return(pc / 7);
}

