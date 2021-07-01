# TODO

<!-- check mark is U+2713 -->

- ✓ read lyrics to determine syllable count and penult length
- ✓ set range per voiceName 
- ✓ add syllables/lyrics
- ✓ combine Choruses into one large structure
- ✓ fix problems with rperm selection:
    - Fortuna is generating rperms in the range 0-3 but actually there are
      variable length rpermtables for duple and the two kinds of triple
    - One option would be to have Fortuna generate within a standard,
      too-large range, and then have a "normalization" function that loops the
      index around if it is too large for the vector (like a modulo)
    - Another option is to build in the vector length information to the ark,
        so instead of using fromList2D as-is, use a data structure that knows
        its own length once constructed (like we do for Sentence and others)
    - (*I did both ideas*)

- ✓ set style, mode, meter from input file and pass as one structure throughout
- ✓ adjust for mode
    x add key signature
    x add ficta/inflected notes
        - We are just taking his sharp or flat scale notes and putting ficta
          on those notes every time. We are not doing any actual ficta
          application by rule. 
- ✓ - Fix out-of-range notes: (you are setting notes per voice with knowledge of
   mode, so you should be able to bring in not just an octave per voice
   (voice2octave) but a range and check that the note is in that range.
   Change octave if too high or too low.
- ✓ avoid too-large leaps
    - In a long stepwise descent if it goes below range the adjustment
      algorithm will result in a seventh when it just can't keep going down.
    - Ideally you would look at the whole phrase and do some kind of badness
      score to evaluate whether to shift the whole phrase.
    - Or, get into Kircher's recommendations to swap voice parts when they go
      out of range (would also depend on a badness score).

- ✓ check transitions between choruses

- ✓ read text from file including title, changes of meter or  style

# Adjustments (confirm we are doing as he recommends)

- avoid out-of-range by swapping voices (see above)
- avoid forbidden intervals 
- avoid forbidden parallels
- avoid too-large intervals between voices
- avoid voice crossings
- add musica ficta (see above)
- repeating notes for multisyllabic words
- mutation between tonoi/modes (Kircher II: 72 among other places)

- check that each phrase has right number of syllables for that pinax

# Expand to more /pinakes/
- ✓ add new rules for determining pinax and column dependent on textMeter
- ✓ add ability to change column per order of verse lines
- ✓ add sample texts in the necessary meters

# Expand to more /syntagmata/
- ✓ syntagma 2 uses four-voice rpermChoirs:
    x adjust data structure in syntagma 1 for this 
    x write procedure to call these correctly in syntagma 2
- set up syntagma 2 to use strophe numbers in all pinakes

## Revise syllabification/underlay to allow for syntagma 2

- ✓ how to align syllables in syntagma 2 (not sure Kircher says how)
    - will need to RESTRUCTURE how you make Symphonia so that you have
      matching lists (pairs?) of Choruses and Lyrics, one per phrase!
- ✓ check and fill in documentation for Lectio, how text is being read, parsed
- ✓ check and fill in docs for Cogito:
    - ✓ Can we set each phrase of music at a time and keep it in its own
      structure?
    - ✓ Are we setting each phrase to its own set of random perm numbers?
    - ✓ In the end can we have lists of Note structures containing both Pitch
      and LyricSyllable as in MEI? (one list per voice)

# Validate input

- Check for valid input parameters (e.g., Florid can only be Duple)

# Add MEI output module (Scribo/MEI)
- make key signature, time signatures display in Verovio output
- get MIDI working in Verovio web app

- reconfigure old Lilypond module for new setup, or scrap it?

# Web interface
- Add PDF output
- Add MIDI output and playback

# Optimization

- ✓ clarify variable names
- ✓ expand and improve documentation
- ✓ use safe list and vector indexing: (!?) 
    -- needed? 
    -- better way to use `Maybe`?
- check user XML input (validate vs DTD)




