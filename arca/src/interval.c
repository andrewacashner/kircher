/* interval.c 
 * Module for calculating and adjusting pitches with octaves */

#include "interval.h"

range natural_range = {
    {
        /* Voice ranges with untransposed clef arrangement */
        { {pcB, 3}, {pcE, 5} },
        { {pcE, 3}, {pcA, 4} },
        { {pcC, 3}, {pcF, 4} },
        { {pcF, 2}, {pcB, 3} }
    }
};
range_ptr natural_range_ptr = &natural_range;


pitch_octave_ptr range_max(pitch_octave_ptr p8, range_ptr range, int voice) {
    assert(range != NULL && p8 != NULL 
            && voice >= 0 && voice < MAX_VOICE);

    p8->pitch = range->array[voice][1].pitch;
    p8->octave = range->array[voice][1].octave;
    return(p8);
}

musarithm_ptr musarithm_create(void) {
    musarithm_ptr new = malloc(sizeof(musarithm));
    return(new);
}

int vperm_num_to_std_pitch(col_ptr col, int vperm_index, int voice, int note, int octave) {
    int pitch = get_pitch_num(col, vperm_index, voice, note) - 1; /* 0 index */
    return(std_pitch_num(pitch, octave));
}

musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index) {
    /* Copy vperm to musarithm structure and adjust too-large intervals */
    int voice, note; /* indices */
    int pitch1, pitch2, interval;
    int factor;
    int syl; 
    pitch_octave max_p8;
    pitch_octave_ptr max_p8_ptr = &max_p8;
    int max_std;
    int octave[MAX_VOICE];
    assert(music != NULL && col != NULL && vperm_index < VPERM_Z);

    syl = col->syl;
    music->syl = syl;
    
    for (voice = 0; voice < MAX_VOICE; ++voice) {
        /* Find highest possible starting octave for each voice */
        max_p8_ptr = range_max(max_p8_ptr, natural_range_ptr, voice);
        max_std = std_pitch_num(max_p8_ptr->pitch, max_p8_ptr->octave);
        octave[voice] = max_p8_ptr->octave;
       
        /* Check whole set of notes: If any are above the max, move the octave
         * down for all */
        for (note = 0; note < syl; ++note) {
            pitch1 = vperm_num_to_std_pitch(col, vperm_index, voice, note, octave[voice]);
            if (pitch1 > max_std) {
                --octave[voice];
            }
        }
        /* Check set again, now in pairs: Reduce too-large intervals and store
         * in musarithm array */
        for (note = 1; note < syl; ++note) {
            pitch1 = vperm_num_to_std_pitch(col, vperm_index, voice, note - 1, octave[voice]);
            pitch2 = vperm_num_to_std_pitch(col, vperm_index, voice, note, octave[voice]);
            
            /* If the interval is too great between the two notes,
             * shift the second one up or down depending on the direction */
            if (pitch2 - pitch1 > 5) {
                pitch2 -= 7;
            } else if (pitch1 - pitch2 > 5) {
                pitch1 -= 7;
            }

            /* Store adjusted pitches in musarithm array */
            music->array[voice][note - 1] = pitch1;
            music->array[voice][note] = pitch2;
        }
    }
    return(music);
}
/* Given 0-indexed pitch class and Helmholtz octave, return universal diatonic
 * pitch number; pitch class can be negative (e.g., for scale degree 7) */
int std_pitch_num(int pitch_class, int octave) {
   
    assert(pitch_class >= 0 && pitch_class < MAX_PITCH_CLASS);
    assert(octave > 1 && octave < 6);

    return(octave * 7 + pitch_class);
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

int mus_get_octave(musarithm_ptr mus, int voice, int x) {
    int pitch = mus_get_pitch(mus, voice, x);
    return(pitch / 7);
}

int octave_ticks(int octave) {
    return(octave - 4);
}
