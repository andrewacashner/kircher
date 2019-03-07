/* cogito.c 
 * Andrew A. Cashner
 * 2019/02/27
 *
 * Structures and functions for calculating, adjusting musical pitches and
 * intervals 
 */

#include "cogito.h"

note_ptr note_create(void) {
    note_ptr new_note = malloc(sizeof(note));
    return(new_note);
}

note_ptr note_set(note_ptr note, int pnum, int oct, int accid, int dur) {
    if (note == NULL) {
        note = note_create();
    }
    debug_print("note_set", "note", (long int)note);
    debug_print("note_set", "dur", dur);
    note->type = PITCH;
    note->pnum = pnum;
    note->oct = oct;
    note->accid = accid;
    note->accid_type = DEFAULT;
    note->dur = dur;
    note->next = NULL;
    note = note_normalize(note);
    return(note);
}

note_ptr rest_set(note_ptr note, int dur) {
    if (note == NULL) {
        note = note_create();
    }
    debug_print("note_set", "note", (long int)note);
    debug_print("note_set", "dur", dur);
    note->type = REST;
    note->pnum = BLANK;
    note->oct = BLANK;
    note->accid = BLANK;
    note->accid_type = BLANK;
    note->dur = dur;
    note->next = NULL;
    return(note);
}

note_ptr note_last(note_ptr ls) {
    if (ls->next != NULL) {
        ls = note_last(ls->next);
    }
    return(ls);
}

note_ptr note_append(note_ptr ls, note_ptr new) {
    note_ptr head = ls;
    assert(new != NULL);
    debug_print("note_append", "ls", (long int)ls);
    debug_print("note_append", "new", (long int)new);
    if (ls == NULL) {
        head = new;
    } else {
        note_last(ls)->next = new;
    }
    return(head);
}


void note_free(note_ptr ls) {
    if (ls == NULL) {
        free(ls); 
    } else {
        note_free(ls->next);
    }
    return;
}


void note_apply(void (*fn)(note_ptr note), note_ptr music) {
    if (music != NULL) {
        fn(music);
        note_apply(fn, music->next);
    }
    return;
}
note_ptr note_map(note_ptr (*fn)(note_ptr note), note_ptr music) {
    note_ptr head = music;
    music = note_map_inner(fn, music);
    return(head);
}
note_ptr note_map_inner(note_ptr (*fn)(note_ptr note), note_ptr music) {
    if (music != NULL) {
        music = fn(music);
        note_map_inner(fn, music->next);
    }
    return(music);
}
        

note_ptr note_normalize(note_ptr note) {
    assert(note != NULL);

    if (note->pnum > MAX_PNUM) {
        note->oct += note->pnum / PNUM_BASE;
    } else if (note->pnum < MIN_PNUM) {
        note->oct -= note->pnum / PNUM_BASE;
    }

    note->pnum %= PNUM_BASE;

    if (note->accid < FL) {
        note->accid = FL;
    } else if (note->accid > SH) {
        note->accid = SH;
    }

    return(note);
}

note_ptr note_oct_shift(note_ptr note, int dir) {
    assert(note != NULL);
    assert(dir == DIR_DOWN || dir == DIR_UP);
    note->oct += 1 * dir;
    return(note);
}

note_ptr note_oct_lower(note_ptr note) {
    return(note_oct_shift(note, DIR_DOWN));
}
note_ptr note_oct_higher(note_ptr note) {
    return(note_oct_shift(note, DIR_UP));
}
int std_pnum(int oct, int pnum) {
    return(oct * PNUM_BASE + pnum);
}
int note_to_std_pnum(note_ptr note) {
    assert(note != NULL);
    assert(note->type != REST);
    return(std_pnum(note->oct, note->pnum));
}

int note_arithmetic(int (*fn)(int a, int b), note_ptr n1, note_ptr n2) {
    int n1_std, n2_std;
    assert(n1 != NULL);
    assert(n2 != NULL);

    n1_std = note_to_std_pnum(n1);
    n2_std = note_to_std_pnum(n2);
    return(fn(n1_std, n2_std));
}
int subtract(int a, int b) {
    return(a - b);
}
int note_diff(note_ptr n1, note_ptr n2) {
    assert(n1 != NULL && n2 != NULL);
    assert(n1->type != REST && n2->type != REST);
    return(note_arithmetic(subtract, n1, n2));
}

int note_cmp(note_ptr n1, note_ptr n2) {
    int result = 0;
    int test;
    assert(n1 != NULL);
    assert(n2 != NULL);
    assert(n1->type != REST);
    assert(n2->type != REST);

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

chorus_ptr chorus_create(void) {
    int i;
    chorus_ptr choir = malloc(sizeof(chorus));
    for (i = 0; i < MAX_VOICE; ++i) {
        choir->music[i] = NULL;
    }
    return(choir);
}

note_ptr select_voice(chorus_ptr chorus, int voice) {
    assert(chorus != NULL);
    assert(voice >= 0); 
    assert(voice <= MAX_VOICE);
    return(chorus->music[voice]);
}

void chorus_free(chorus_ptr choir) {
    int i;
    for (i = 0; i < MAX_VOICE; ++i) {
        if (select_voice(choir, i) != NULL) {
            note_free(select_voice(choir, i));
        }
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
    note_ptr curr_music = NULL;
    textlist_ptr curr_lyrics = NULL;
    char error_msg[MAX_LINE];

    assert(chorus != NULL);
    assert(text != NULL);
    assert(syntagma != NULL);

    for (curr_lyrics = text; curr_lyrics != NULL; 
            curr_lyrics = curr_lyrics->next) {

        syllables = curr_lyrics->syllables;
        penult_len = curr_lyrics->penult_len;

        debug_print("chorus_compose", "syllables", syllables);
       
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
        for (i = 0; i < MAX_VOICE; ++i) {
            debug_print("chorus_compose", "creating voice", i);
            curr_music = NULL;
            curr_music = voice_compose(curr_music, i, col, mode, vperm_index, 
                    meter, rperm_index);
            chorus->music[i] = note_append(chorus->music[i], curr_music);
        }
    }

    for (i = 0; i < MAX_VOICE; ++i) {
        chorus->music[i] = notelist_adj_interval(chorus->music[i]);
        chorus->music[i] = notelist_adj_accid(chorus->music[i], mode);
        chorus->music[i] = notelist_adj_oct(chorus->music[i], i);
        chorus->music[i] = notelist_adj_leaps(chorus->music[i]);
        chorus->music[i] = notelist_adj_interval(chorus->music[i]);
    }
    chorus = chorus_adj_voice_distance(chorus);
    chorus = chorus_adj_accid(chorus);
    for (i = 0; i < MAX_VOICE; ++i) {
        chorus->music[i] = notelist_adj_interval(chorus->music[i]);
    }

    return(chorus);
}

note_ptr voice_compose(note_ptr ls, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index) {

    assert(col != NULL);
    assert(voice >= 0); 
    assert(voice <= MAX_VOICE);
    /* check other params */

    ls = arca_to_notelist(ls, voice, col, mode, vperm_index, 
            rperm_type, rperm_index);
    ls = notelist_adj_accid(ls, mode);
    return(ls);
}

note_ptr arca_to_notelist(note_ptr music, int voice, col_ptr col, int mode, 
        int vperm_index, int rperm_type, int rperm_index) {

    int x, r;
    int pnum;
    int octave_max[] = { 5, 4, 4, 3 };
    int oct = octave_max[voice];
    int accid = NA;    
    int dur;
    note_ptr note = NULL;

    assert(col != NULL);
    assert(voice >= 0); 
    assert(voice <= MAX_VOICE);
    /*other params */

    r = x = 0;
    while (r < RPERM_X && x < col->syl) {
        note = NULL;

        /* Get just the rhythm if it is a rest;
         * if so move to next rhythm but keep same pitch */
        dur = get_value_num(col, rperm_type, rperm_index, r);
        if (dur < MIN_REST) {
            /* Rhythm != rest, store pitch + rhythm, move to next */
            pnum = get_pitch_num(col, vperm_index, voice, x);
            pnum = pnum_in_mode(pnum, mode);
            debug_print("arca_to_notelist", "pnum", pnum);
            
            note = note_set(note, pnum, oct, accid, dur);
            music = note_append(music, note);
            ++x, ++r;

        } else {
            /* Rhythm is rest, just store rhythm in rest and match current pitch (x)
             * to next rhythm (r) */
            debug_print("arca_to_notelist rest", "dur", dur);
            note = rest_set(note, dur);
            music = note_append(music, note);
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
    assert(pnum >= MIN_PNUM && pnum <= PNUM_BASE); /* note 7 IS allowed */
    assert(mode >= 0 && mode <= MAX_MODE);
    return(pnum + mode_offset[mode]);
}


note_ptr notelist_adj_oct(note_ptr music, int voice) {
    note_ptr curr = NULL;
    int test;
    note range_max[] = { 
        { PITCH, pcG, 5, 0, 0 },
        { PITCH, pcD, 5, 0, 0 },
        { PITCH, pcF, 4, 0, 0 },
        { PITCH, pcC, 4, 0, 0 }
    };
    note range_min[] = {
        { PITCH, pcC, 4, 0, 0 },
        { PITCH, pcG, 3, 0, 0 },
        { PITCH, pcB, 2, 0, 0 },
        { PITCH, pcE, 2, 0, 0 }
    };

    assert(music != NULL);

    for (curr = music; curr != NULL; curr = curr->next) {
        if (curr->type != REST) {
            test = note_cmp(curr, &range_max[voice]);
            if (test > 0) {
                curr = note_oct_lower(curr);
            } 
            test = note_cmp(curr, &range_min[voice]);
            if (test < 0) {
                curr = note_oct_higher(curr);
            }
        }
    }

    return(music);
}

note_ptr notelist_adj_accid(note_ptr music, int mode) {
    note_ptr n1 = NULL;
    note_ptr n2 = NULL;
    note_ptr curr = NULL;
    int test;
    
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

    assert(music != NULL);

    for (curr = music; curr->next != NULL; curr = curr->next) {
        if (curr->type != REST) {
            n1 = curr;
            n2 = curr->next;
            if (mode_ficta_tf[mode] == true) {
                n1 = ficta(n1, n2, mode);
            }
            if (mode_mollis_tf[mode] == true) {
                n1 = note_accid_test_set(n1, pcB, FL, SIGNATURE);
                n2 = note_accid_test_set(n2, pcB, FL, SIGNATURE);
            }
            if (mode == mode_sharp3) {
                n1 = note_accid_mode_test_set(n1, 2, mode, SH, DEFAULT);
                n2 = note_accid_mode_test_set(n2, 2, mode, SH, DEFAULT);
            }
        }
    }
    for (curr = music; curr->next != NULL; curr = curr->next) {
        n1 = curr;
        n2 = curr->next;
        if (n1->type != REST && n2->type != REST) {
            test = note_diff(n1, n2);
            if (test == 0) {
                /* If two repeated pnums have diff accidentals, use the 
                 * second one
                 */
                if (n1->accid != n2->accid) {
                    n1 = note_accid_cpy(n1, n2);
                }
            }
        }
    }
    return(music);
}


note_ptr note_accid_set(note_ptr note, int accid, int accid_type) {
    assert(note != NULL);
    assert(accid >= FL); 
    assert(accid <= SH); 
    assert(accid_type >= ACCID_DEFAULT);
    assert(accid_type < MAX_ACCID_TYPE);
    
    note->accid = accid;
    note->accid_type = accid_type;
    return(note);
}
note_ptr note_accid_cpy(note_ptr n1, note_ptr n2) {
    assert(n1 != NULL);
    assert(n2 != NULL);
    n1 = note_accid_set(n1, n2->accid, n2->accid_type);
    return(n1);
}
note_ptr note_accid_test_set(note_ptr note, int pnum, 
        int accid, int accid_type) {
    assert(note != NULL);
    if (note->pnum == pnum) {
        note = note_accid_set(note, accid, accid_type);
    }
    return(note);
}   
note_ptr note_accid_mode_test_set(note_ptr note, int pnum, int mode, 
        int accid, int accid_type) {
    assert(note != NULL);
    assert(mode >= 0 && mode <= MAX_MODE);
    if (note->pnum == mode_scale_deg(pnum, mode)) {
        note = note_accid_set(note, accid, accid_type);
    }
    return(note);
}

note_ptr ficta(note_ptr n1, note_ptr n2, int mode) {
    int pnum1, pnum2;
    assert(n1 != NULL);
    assert(n2 != NULL);
    
    pnum1 = n1->pnum;
    pnum2 = n2->pnum;

    if (pnum1 == mode_scale_deg(6, mode) && 
            pnum2 == mode_scale_deg(5, mode)) {
        /* lower ^6 if descending */
        if (pnum1 != pcF) {
            n1 = note_accid_set(n1, FL, FICTA);
        }
    } else if (pnum1 == mode_scale_deg(7, mode) && 
            pnum2 == mode_scale_deg(1, mode)) {
        /* raise ^7 if ascending */
        n1 = note_accid_set(n1, SH, FICTA);
    } 
    return(n1);
}

int mode_scale_deg(int scale_deg, int mode) {
    return(pnum_in_mode(scale_deg - 1, mode) % PNUM_BASE);
}
note_ptr notelist_adj_interval(note_ptr music) {
    note_ptr curr, n1, n2;
    int test;

    assert(music != NULL);

    for (curr = music; curr->next != NULL; curr = curr->next) {
        n1 = curr;
        n2 = curr->next;
        if (n1->type != REST && n2->type != REST) {
            test = note_diff(n1, n2);
            if (test > MAX_INTERVAL) {
                /* If the first note is too far above the second, lower the first;
                 * if the first note is too far below the second, lower the second
                 */
                n1 = note_oct_lower(n1);
            } else if (test < -MAX_INTERVAL) {
                n2 = note_oct_lower(n2);
            }
        }
    }
    return(music);
}

note_ptr notelist_adj_leaps(note_ptr music) {
    note_ptr curr, n1, n2, n3;
    int test1, test2;
    assert(music != NULL);

    for (curr = music; curr->next->next != NULL; curr = curr->next) {
            n1 = curr;
            n2 = curr->next;
            n3 = curr->next->next;
            if (n1->type != REST && 
                    n2->type != REST &&
                    n3->type != REST) {

                test1 = note_diff(n1, n2);
                test2 = note_diff(n2, n3);
                if (test1 > MAX_INTERVAL - 1 && test2 > 0) {
                    n2 = note_oct_shift(n2, DIR_UP);
                    n3 = note_oct_shift(n3, DIR_UP);
                } else if (test1 < -1 * (MAX_INTERVAL - 1) && test2 < 0) {
                    n2 = note_oct_shift(n2, DIR_DOWN);
                    n3 = note_oct_shift(n3, DIR_DOWN);
            }
        }
    }
    return(music);
}
/* TODO consolidate this with above function? */

note_ptr notelist_ref(note_ptr ls, int index) {
    assert(ls != NULL);
    assert(index >= 0);

    for (; index >= 0 && ls != NULL; --index) {
        if (ls ->next != NULL) {
            ls = ls->next;
        } 
    }
    return(ls);
}

int notelist_len(note_ptr ls) {
    int i;
    for (i = 0; ls != NULL; ls = ls->next, ++i) {
        ; /* just count */
    }
    return(i);
}

chorus_ptr voice_swap(chorus_ptr choir, int upper, int lower) {
    note_ptr tmp = NULL;
    assert(choir != NULL);
    /* Swap the notes in two voices but keep them in their original octaves:
     * e.g., d5/b3 becomes b5/d3 */
    tmp = choir->music[upper];
    tmp = note_map(note_oct_lower, tmp);

    choir->music[upper] = choir->music[lower];
    choir->music[upper] = note_map(note_oct_higher, choir->music[upper]);

    choir->music[lower] = tmp;

    return(choir);
}

chorus_ptr chorus_adj_voice_distance(chorus_ptr choir) {
    note_ptr upper, lower;
    int i, test;
    upper = lower = NULL;
    
    assert(choir != NULL);
    for (i = 0; i < MAX_VOICE; ++i) {
        assert(choir->music[i] != NULL);
    }

    /* Check voices from bass upwards */
    for (i = TENOR; i > CANTUS; --i) {
        for (upper = choir->music[i - 1],
                lower = choir->music[i];

                upper != NULL &&
                lower != NULL;

                upper = upper->next,
                lower = lower->next) {

            if (upper->type != REST &&
                    lower->type != REST) {

                test = note_diff(upper, lower);
                if (test > MAX_VOICE_DISTANCE) {
                    choir = voice_swap(choir, i - 1, i); 
                    break;
                }
            }
        }
    }
    return(choir);
}
/* TODO swap voices? */

chorus_ptr chorus_adj_accid(chorus_ptr choir) {
    note_ptr lower, mid, upper;
    note_ptr voice, note1, note2;
    int i, v, n, cf, test;
    lower = mid = upper = voice = note1 = note2 = NULL;

    for (i = CANTUS; i < BASSUS; ++i) {
        for (lower = choir->music[BASSUS],
                upper = choir->music[i];

                lower != NULL &&
                upper != NULL;

                lower = lower->next,
                upper = upper->next) {

            if (upper->pnum == lower->pnum) {
                if (upper->accid != lower->accid) {

                    upper->accid = lower->accid;
                    upper->accid_type = lower->accid_type;
                }
            } 
        }
    }


    for (v = 0; v < 4; ++v) {
        voice = select_voice(choir, v);
        for (n = 0; n < notelist_len(voice); ++n) {
            note1 = notelist_ref(voice, n);
            if (note1->accid_type == FICTA) {
                for (cf = 0; cf < 4; ++cf) {
                    if (cf != v) {
                        note2 = notelist_ref(select_voice(choir, cf), n);
                        test = abs(note_diff(note1, note2)) % 7;
                        if (test == 4 || test == 5) {
                            if (note1->pnum == pcE &&
                                    note2->pnum == pcB &&
                                    note2->accid_type == SIGNATURE) {
                                /* don't adjust */
                            } else {
                                note1->accid = NA;
                                note1->accid_type = DEFAULT;
                            }
                            break;
                        }
                    }
                }
            }
        }
    }
    
    return(choir);
}

