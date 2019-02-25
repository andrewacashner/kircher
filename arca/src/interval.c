/* interval.c 
 * Module for calculating and adjusting pitches with octaves */

#include "interval.h"


range natural_range = {
    {
        /* Voice ranges with untransposed clef arrangement */
        { { pcB, 3 }, { pcE, 5 } },
        { { pcE, 3 }, { pcA, 4 } },
        { { pcC, 3 }, { pcF, 4 } },
        { { pcF, 2 }, { pcB, 3 } }
    }
};
range_ptr natural_range_ptr = &natural_range;

pitch_octave_ptr pitch_create(int pitch_class, int octave) {
    pitch_octave_ptr new_pitch_ptr = malloc(sizeof(pitch_octave));
    new_pitch_ptr->pitch_class = pitch_class;
    new_pitch_ptr->octave = octave;
    return(new_pitch_ptr);
}

pitch_octave_ptr get_range(range_ptr range, int range_type, int voice) {
    assert(range != NULL && voice >= 0 && voice < MAX_VOICE);
    assert(range_type >= RANGE_MIN && range_type < MAX_RANGE_TYPE);

    return(&range->array[voice][range_type]);
}

musarithm_ptr musarithm_create(void) {
    musarithm_ptr new = malloc(sizeof(musarithm));
    return(new);
}

musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index,
        int mode_num) {
    /* Copy vperm to musarithm structure and adjust too-large intervals */
    int voice, note; /* indices */
    int pitch1, pitch2;
    int syl; 
    pitch_octave_ptr max_p8_ptr = NULL;
    int octave[MAX_VOICE];

    assert(music != NULL && col != NULL && vperm_index < VPERM_Z);

    syl = col->syl;
    music->syl = syl;
    
    for (voice = 0; voice < MAX_VOICE; ++voice) {
        /* Find highest possible starting octave for each voice */
        max_p8_ptr = get_range(natural_range_ptr, RANGE_MAX, voice);
        octave[voice] = max_p8_ptr->octave;
       
        /* Set octave: 
         * Check whole set of notes, if any are above the max, move the octave
         * down for all */
        for (note = 0; note < syl; ++note) {
            pitch1 = get_pitch_num(col, vperm_index, voice, note);
            pitch1 += mode[mode_num][0]; /* set to mode */
            if (pitch1 > max_p8_ptr->pitch_class) {
                --octave[voice];
                break;
            } 
        }
        /* Reduce intervals:
         * Check set again, now in pairs: Reduce too-large intervals and store
         * in musarithm array */
        for (note = 1; note < syl; ++note) {
            pitch1 = get_pitch_num(col, vperm_index, voice, note - 1);
            pitch2 = get_pitch_num(col, vperm_index, voice, note);
            
            /* If the interval is too great between the two notes,
             * shift the second one up or down depending on the direction */
            if (pitch2 - pitch1 > 5) {
                pitch2 -= 7;
            } else if (pitch1 - pitch2 > 5) {
                pitch1 -= 7;
            }

            /* Store adjusted pitches in musarithm array */
            music->array[voice][note - 1] = pitch_create(pitch1, octave[voice]);
            music->array[voice][note] = pitch_create(pitch2, octave[voice]);
        }
    }
    return(music);
}

int mus_get_pitch_class(musarithm_ptr mus, int voice, int x) {
    return(mus->array[voice][x]->pitch_class);
}

int mus_get_octave(musarithm_ptr mus, int voice, int x) {
    return(mus->array[voice][x]->octave);
}

int octave_ticks(int octave) {
    return(octave - 3);
}
