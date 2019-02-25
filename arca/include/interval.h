/* interval.h */

#include <assert.h>
#include <stdlib.h>
#include "arca.h"

#define MAX_VOICE 4

/* DATA STRUCTURES */
typedef struct musarithm {
    int syl;
    int array[VPERM_Y][VPERM_X];
} musarithm;
typedef musarithm *musarithm_ptr;

/* FUNCTION PROTOTYPES */
musarithm_ptr musarithm_create(void);
musarithm_ptr musarithm_set(musarithm_ptr music, col_ptr col, int vperm_index,
        int mode_num);
int mus_get_pitch(musarithm_ptr mus, int voice, int x);
int mus_get_pitch_class(musarithm_ptr mus, int voice, 
        int x, int mode_num);
int mus_get_octave(musarithm_ptr mus, int voice, 
        int x, int mode_num);
int octave_ticks(int octave);

char *std_pitch_to_ly(char *str, int std_pitch);
