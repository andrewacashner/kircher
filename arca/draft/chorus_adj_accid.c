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

