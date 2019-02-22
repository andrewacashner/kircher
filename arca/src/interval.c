/* interval.c 
 * Module for calculating and adjusting pitches with octaves */

#include "interval.h"

range natural_range = {
    {
        /* Voice ranges with untransposed clef arrangement */
        { STD_PITCH(nB, 3), STD_PITCH(nE, 5) },
        { STD_PITCH(nE, 3), STD_PITCH(nA, 4) },
        { STD_PITCH(nC, 3), STD_PITCH(nF, 4) },
        { STD_PITCH(nF, 2), STD_PITCH(nB, 3) }
    }
};
range_ptr natural_range_ptr = &natural_range;


int range_max(range_ptr r, int voice) {
    return(r->array[voice][1]);
}

int range_min(range_ptr r, int voice) {
    return(r->array[voice][0]);
}

musarithm_ptr musarithm_create(void) {
    musarithm_ptr new = malloc(sizeof(musarithm));
    return(new);
}

musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index) {
    /* Copy vperm to musarithm structure and adjust too-large intervals */
    int voice, note; /* indices */
    int pitch1, pitch2, interval;
    int factor;
    int syl; 
    assert(music != NULL && col != NULL && vperm_index < VPERM_Z);

    syl = col->syl;
    music->syl = syl;

    for (voice = 0; voice < MAX_VOICE; ++voice) {
        for (note = 1; note < syl; note = note + 2) {
            pitch1 = get_pitch_num(col, vperm_index, voice, note - 1);
            pitch2 = get_pitch_num(col, vperm_index, voice, note);
            printf("DEBUG musarithm_set: pitch1 %d, pitch2 %d\n", pitch1, pitch2);
            if (pitch2 > pitch1) {
                interval = pitch2 - pitch1;
                factor = -1;
            } else {
                interval = pitch1 - pitch2;
                factor = 1;
            };
            if (interval > 5) {
                pitch2 = pitch2 + 7 * factor;
            }
            /* --pitch1, --pitch2; */ /* 0-index */ /* TODO check */
            music->array[voice][note - 1] = pitch1;
            music->array[voice][note] = pitch2;
        }
    }
    return(music);
}

/* Given 0-indexed pitch class and Helmholtz octave, return universal diatonic
 * pitch number; pitch class can be negative (e.g., for scale degree 7) */
int std_pitch_num(int pitch_class, int octave) {
    
    assert(pitch_class >= 0 && pitch_class < MAX_PITCH &&
            octave >= 2 && octave < 7);

    return(octave * 7 + pitch_class);
}

int octave_set(musarithm_ptr mus, int voice) {
    int i, test;
    int max = range_max(natural_range_ptr, voice);
    int octave = max / 7;

    assert(mus != NULL 
            && voice >= 0 && voice < MAX_VOICE);

    for (i = 0; i < mus->syl; ++i) {
        test = mus_get_pitch(mus, voice, i);
        if (std_pitch_num(test, octave) > max) {
            --octave;
            break;
        }
    }
    return(octave);
}

int mus_get_pitch(musarithm_ptr mus, int voice, int x) {
    assert(x < mus->syl 
            && voice >= 0 && voice < MAX_VOICE && 
            mus != NULL);
    return(mus->array[voice][x]);
}

int mus_get_pitch_class(musarithm_ptr mus, int voice, int x) {
    int pitch = mus_get_pitch(mus, voice, x);
    return(pitch % 7);
}

musarithm_ptr mus_arrange_voices(musarithm_ptr mus) {
    /* Put voices in proper octaves */
    int i, j, n;
    int octave[MAX_VOICE];
    assert(mus != NULL);

    for (i = 0; i < MAX_VOICE; ++i) {
        octave[i] = octave_set(mus, i);
    }
    for (i = 1; i < MAX_VOICE; ++i) {
        if (mus_get_pitch(mus, i - 1, 0) - mus_get_pitch(mus, i, 0) > 7 ) {
            --octave[i - 1];
        }
    }
    for (i = 0; i < MAX_VOICE; ++i) {
        for (j = 0; j < mus->syl; ++j) {
            /* Replace pitch class in mus->array[i][j] with standard pitch num */
            n = mus_get_pitch(mus, i, j);
            mus->array[i][j] = std_pitch_num(n, octave[i]);
        }
    }
    return(mus);
}

