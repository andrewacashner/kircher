/* interval.h */

#include <assert.h>
#include <stdlib.h>
#include "arca.h"

#define MAX_VOICE 4

enum PITCH_CLASS_NUMS {
    pcC = 0, pcD, pcE, pcF, pcG, pcA, pcB, pcC8, MAX_PITCH_CLASS
};
extern enum PITCH_CLASS_NUMS pitch_class_nums;

enum RANGE_TYPE {
    RANGE_MIN, RANGE_MAX, MAX_RANGE_TYPE
};
extern enum RANGE_TYPE range_type;



/* DATA STRUCTURES */
typedef struct musarithm {
    int syl;
    int array[VPERM_Y][VPERM_X];
} musarithm;
typedef musarithm *musarithm_ptr;

typedef struct pitch_octave {
    int pitch;
    int octave;
} pitch_octave;
typedef pitch_octave *pitch_octave_ptr;

typedef struct range {
    /* 4 voices, min and max pitches */
    pitch_octave array[4][2];
} range;
typedef range *range_ptr;

/* FUNCTION PROTOTYPES */
pitch_octave_ptr get_range(pitch_octave_ptr p8, range_ptr range,
        int range_type, int voice);
musarithm_ptr musarithm_create(void);
musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index);
int std_pitch_num(int pitch_class, int octave);
int vperm_num_to_std_pitch(col_ptr col, int vperm_index, int voice, int note, int octave);
int mus_get_pitch(musarithm_ptr mus, int voice, int x);
int mus_get_pitch_class(musarithm_ptr mus, int voice, int x);
int mus_get_octave(musarithm_ptr mus, int voice, int x);
int octave_ticks(int octave);

/* VARIABLES */
extern range natural_range;
extern range_ptr natural_range_ptr;

