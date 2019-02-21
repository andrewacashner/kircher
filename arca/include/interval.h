/* interval.h */

#include <assert.h>
#include <stdlib.h>
#include "arca.h"

#define STD_PITCH(n, o) o * 7 + n
#define MAX_VOICE 4

/* DATA STRUCTURES */
typedef struct musarithm {
    int syl;
    int array[VPERM_Y][VPERM_X];
} musarithm;
typedef musarithm *musarithm_ptr;

typedef struct range {
    int array[4][2];
} range;
typedef range *range_ptr;

/* FUNCTION PROTOTYPES */
int range_max(range_ptr r, int voice);
int range_min(range_ptr r, int voice);
musarithm_ptr musarithm_create(void);
musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index);
int std_pitch_num(int pitch_class, int octave);
int octave_set(musarithm_ptr mus, int voice);
int mus_get_pitch(musarithm_ptr mus, int voice, int x);
int mus_get_pitch_class(musarithm_ptr mus, int voice, int x);
musarithm_ptr mus_set_pitch_num(musarithm_ptr mus);

/* VARIABLES */
extern range natural_range;
extern range_ptr natural_range_ptr;

