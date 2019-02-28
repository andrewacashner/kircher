/* cogito.c 
 * Andrew A. Cashner
 * 2019/02/27
 *
 * Structures and functions for calculating, adjusting musical pitches and
 * intervals 
 */

#include "cogito.h"

note_ptr note_create(void) {
    note_ptr note = malloc(sizeof(note));
    return(note);
}

note_ptr note_set(note_ptr note, int pnum, int oct, int accid, int dur) {
    assert(note != NULL);
    note->pnum = pnum;
    note->oct = oct;
    note->accid = accid;
    note->dur = dur;

    note = note_normalize(note);
    return(note);
}


note_ptr note_normalize(note_ptr note) {
    assert(note != NULL);

    if (note->pnum > 7) {
        note->oct += note->pnum / 7;
    } else if (note->pnum < 0) {
        note->oct -= note->pnum / 7;
    }

    note->pnum %= 7;

    if (note->accid < FL) {
        note->accid = FL;
    } else if (note->accid > SH) {
        note->accid = SH;
    }

    if (note->dur < BRD) { /* XXX or error */
        note->dur = BRD;
    } else if (note->dur > FS) {
        note->dur = FS;
    }

    return(note);
}

note_ptr note_oct_shift(note_ptr note, int dir) {
    assert(note != NULL);
    assert(dir == 1 || dir == -1);
    note->oct += 1 * dir;
    return(note);
}

note_ptr note_oct_lower(note_ptr note) {
    return(note_oct_shift(note, -1));
}

int std_pnum(int oct, int pnum) {
    return(oct * 7 + pnum);
}
int note_to_std_pnum(note_ptr note) {
    assert(note != NULL);
    return(std_pnum(note->oct, note->pnum));
}

int note_arithmetic(int (*fn)(int a, int b), note_ptr n1, note_ptr n2) {
    int n1_std, n2_std;
    assert(n1 != NULL && n2 != NULL);
    n1_std = note_to_std_pnum(n1);
    n2_std = note_to_std_pnum(n2);
    return(fn(n1_std, n2_std));
}
int add(int a, int b) {
    return(a + b);
}
int subtract(int a, int b) {
    return(a - b);
}
int note_sum(note_ptr n1, note_ptr n2) {
    return(note_arithmetic(add, n1, n2));
}
int note_diff(note_ptr n1, note_ptr n2) {
    return(note_arithmetic(subtract, n1, n2));
}

int note_cmp(note_ptr n1, note_ptr n2) {
    int result = 0;
    int test;
    assert(n1 != NULL && n2 != NULL);

    test = note_diff(n1, n2);
    if (test == 0) {
        result = 0;
    } else if (test > 0) {
        result = 1;
    } else if (test < 0) {
        result = -1;
    }
    return(result);
}

notelist_ptr notelist_create(void) {
    notelist_ptr ls = malloc(sizeof(notelist));
    ls->note = malloc(sizeof(note));
    return(ls);
}

notelist_ptr notelist_set(notelist_ptr ls, note_ptr note) {
    assert(ls != NULL && note != NULL);
    ls->note = note_set(ls->note, 
            note->pnum, note->oct, note->accid, note->dur);
    ls->next = NULL;
    return(ls);
}

notelist_ptr notelist_last(notelist_ptr ls) {
    if (ls->next == NULL) {
        return(ls);
    } else {
        return(notelist_last(ls->next));
    }
}

notelist_ptr notelist_append(notelist_ptr ls, notelist_ptr new) {
    notelist_ptr head = ls;
    assert(new != NULL);
    if (ls == NULL) {
        head = new;
    } else {
        notelist_last(ls)->next = new;
    }
    return(head);
}

notelist_ptr notelist_set_append(notelist_ptr ls, int pnum, int oct, int accid, int dur) {
    note_ptr note = note_create();
    notelist_ptr music = notelist_create();

    note = note_set(note, pnum, oct, accid, dur);
    music = notelist_set(music, note);

    ls = notelist_append(ls, music);
    return(ls);
}

notelist_ptr notelist_set_append_rest(notelist_ptr ls, int dur) {
    notelist_ptr music = NULL;
    check_ptr(ls);

    music = notelist_set_append(music, REST, REST, REST, dur);
    return(music);
}



void notelist_free(notelist_ptr ls) {
    if (ls != NULL) {
        if (ls->next != NULL) {
            notelist_free(ls->next);
        }
        free(ls->note);
        free(ls);
    }
    return;
}


void notelist_apply(void (*fn)(note_ptr note), notelist_ptr music) {
    if (music != NULL) {
        fn(music->note);
        notelist_apply(fn, music->next);
    }
    return;
}
notelist_ptr notelist_map(note_ptr (*fn)(note_ptr note), notelist_ptr music) {
    notelist_ptr head = music;
    music = notelist_map_inner(fn, music);
    return(head);
}
notelist_ptr notelist_map_inner(note_ptr (*fn)(note_ptr note), notelist_ptr music) {
    if (music != NULL) {
        music->note = fn(music->note);
        notelist_map_inner(fn, music->next);
    }
    return(music);
}
        

chorus_ptr chorus_create(void) {
    int i;
    chorus_ptr choir = malloc(sizeof(chorus));
    for (i = 0; i < MAX_VOICE; ++i) {
        choir->music[i] = notelist_create();
    }
    return(choir);
}

notelist_ptr select_voice(chorus_ptr chorus, int voice) {
    check_ptr(chorus);
    check_voice_range(voice);
    return(chorus->music[voice]);
}

void chorus_free(chorus_ptr choir) {
    int i;
    for (i = 0; i < MAX_VOICE; ++i) {
        free(choir->music[i]);
    }
    free(choir);
    return;
}



chorus_ptr chorus_compose(chorus_ptr chorus, textlist_ptr text, 
        syntagma_ptr syntagma, int mode, int meter) {

    int i, syllables, penult_len;
    int test, vperm_index, rperm_index;
    pinax_ptr pinax = NULL;
    col_ptr col = NULL;
    textlist_ptr curr_lyrics = NULL;
    char error_msg[MAX_LINE];
    int seed = time(NULL);
    srand(seed);

    check_ptr(chorus);
    check_ptr(text);
    check_ptr(syntagma);

    for (curr_lyrics = text; curr_lyrics != NULL; 
            curr_lyrics = curr_lyrics->next) {

        syllables = text->syllables;
        penult_len = text->penult_len;
       
        pinax = get_pinax_ptr_type(syntagma, penult_len);
        
        test = check_mode(pinax, mode);
        if (test != 0) {
            sprintf(error_msg, "%d", mode + 1); /* back to 1-index */
            exit_error(test, error_msg);
        }

        col = get_col_ptr_syl(pinax, syllables);
        vperm_index = select_vperm(col);
        rperm_index = select_rperm(col, meter);

        if (col == NULL) {
            sprintf(error_msg, "%d", syllables);
            exit_error(NO_COL_SYL, error_msg);
        }
    }

    for (i = 0; i < MAX_VOICE; ++i) {
        chorus->music[i] = voice_compose(chorus, i, col, mode, vperm_index, 
                meter, rperm_index);
    }
    return(chorus);
}

notelist_ptr voice_compose(chorus_ptr chorus, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index) {
    notelist_ptr ls = NULL;
    
    check_ptr(chorus);
    check_voice_range(voice);
    check_ptr(chorus->music[voice]);
    check_ptr(col);
    /* check other params */

    ls = chorus->music[voice];
    ls = arca_to_notelist(ls, voice, col, mode, vperm_index, 
            rperm_type, rperm_index);
    ls = notelist_adj_oct(ls, voice);
    ls = notelist_adj_accid(ls, mode);
    ls = notelist_adj_interval(ls);
    return(ls);
}

notelist_ptr arca_to_notelist(notelist_ptr music, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index) {

    int x, r;
    int pnum;
    int octave_max[] = { 5, 4, 4, 3 };
    int oct = octave_max[voice];
    int accid = NA;    
    int dur;

    check_ptr(music);
    check_ptr(col);
    check_voice_range(voice);
    /*other params */

    r = x = 0;
    while (r < RPERM_X && x < col->syl) {
         /* Get just the rhythm if it is a rest;
         * if so move to next rhythm but keep same pitch */
        dur = get_value_num(col, rperm_type, rperm_index, r);
        if (dur < MIN_REST) {
            /* Rhythm != rest, store pitch + rhythm, move to next */
            pnum = get_pitch_num(col, vperm_index, voice, x);
            pnum = pnum_in_mode(pnum, mode);
            music = notelist_set_append(music, pnum, oct, accid, dur);
            ++x, ++r;
        } else {
            /* Rhythm is rest, just store rhythm in rest and match current pitch (x)
             * to next rhythm (r) */
            music = notelist_set_append_rest(music, dur);
            ++r;
        }
    }
    return(music);
}

int pnum_in_mode(int pnum, int mode) {
    int mode_offset[] = { 
        pcD, pcG, pcA, 
        pcA, pcB, pcF, 
        pcG, pcG, pcD, 
        pcA, pcC, pcF,
    };
    assert(mode >= 0 && mode < 12);
    return(pnum + mode_offset[mode]);
}

notelist_ptr notelist_adj_oct(notelist_ptr music, int voice) {
    notelist_ptr curr = NULL;
    int test;
    bool too_high_tf = false;
    note range_max[] = { 
        { pcG, 5, 0, 0 },
        { pcD, 5, 0, 0 },
        { pcF, 4, 0, 0 },
        { pcC, 4, 0, 0 }
    };

    assert(music != NULL);
    assert(voice >= 0 && voice < MAX_VOICE);

    for (curr = music; curr != NULL; curr = curr->next) {
        test = note_cmp(curr->note, &range_max[voice]);
        if (test > 0) {
            too_high_tf = true;
            break;
        }
    }
    if (too_high_tf == true) {
        music = notelist_map(note_oct_lower, music);
    }
    return(music);
}
/* TODO check minimums also */
/* TODO check distance between voice also (& swap voices) */

notelist_ptr notelist_adj_accid(notelist_ptr music, int mode) {
    note_ptr n1 = NULL;
    note_ptr n2 = NULL;
    notelist_ptr curr = NULL;

    bool mode_mollis_tf[] = {
        false, true, false, 
        false, true, true, 
        false, false, true, 
        false, false, true
    };
    bool mode_ficta_tf[] = {
        true, true, true, 
        false, false, false,
        true, true, true,
        false, false, false
    };
    int mode_sharp3 = 3;

    for (curr = music; curr->next != NULL; curr = curr->next) {
        n1 = curr->note;
        n2 = curr->next->note;

        if (mode_mollis_tf[mode] == true) {
            n1 = b_flat(n1);
            n2 = b_flat(n2);
        }
        if (mode == mode_sharp3) {
            n1 = c_sharp(n1);
            n2 = c_sharp(n2);
        }
        if (mode_ficta_tf[mode] == true) {
            n1 = ficta(n1, n2);
        }
    }
    return(music);
}

note_ptr note_accid_set(note_ptr note, int accid) {
    assert(note != NULL);
    assert(accid >= FL && accid <= SH);
    note->accid = accid;
    return(note);
}
note_ptr note_accid_test_set(note_ptr note, int pnum, int accid) {
    if (note->pnum == pnum) {
        note = note_accid_set(note, accid);
    }
    return(note);
}   
note_ptr b_flat(note_ptr note) {
    return(note_accid_test_set(note, pcB, FL));
}
note_ptr c_sharp(note_ptr note) {
    return(note_accid_test_set(note, pcC, SH));
}
note_ptr ficta(note_ptr n1, note_ptr n2) {
    int pnum1 = n1->pnum;
    int pnum2 = n2->pnum;
    int accid = n1->accid;

    if ((pnum1 == pcB && pnum2 == pcA) 
            || (pnum1 == pcE && pnum2 == pcD)) {
        accid = FL;
    } else if ((pnum1 == pcC && pnum2 == pcD) 
            || (pnum1 == pcF && pnum2 == pcG)) {
        accid = SH;
    }
    n1 = note_accid_set(n1, accid);
    return(n1);
}

notelist_ptr notelist_adj_interval(notelist_ptr music) {
    notelist_ptr curr = NULL;
    note_ptr n1, n2;
    int test;
    for (curr = music; curr->next != NULL; curr = curr->next) {
        n1 = curr->note;
        n2 = curr->next->note;
        test = note_diff(n1, n2);
        if (test == 0) {
            /* If two repeated pnums have diff accidentals, use the 
             * second one
             */
            if (n1->accid != n2->accid) {
                n1->accid = n2->accid;
            }
        } else if (test > 5) {
        /* If the first note is too far above the second, lower the first;
         * if the first note is too far below the second, lower the second
         */
            n1 = note_oct_shift(n1, -1);
        } else if (test < -5) {
            n2 = note_oct_shift(n2, -1);
        }
    }
    return(music);
}









