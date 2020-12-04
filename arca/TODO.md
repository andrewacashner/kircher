# TODO

x read lyrics to determine syllable count and penult length
x set range per voiceName 
x add syllables/lyrics
x combine Choruses into one large structure
x fix problems with rperm selection:
    - Fortuna is generating rperms in the range 0-3 but actually there are
      variable length rpermtables for duple and the two kinds of triple
    - One option would be to have Fortuna generate within a standard,
      too-large range, and then have a "normalization" function that loops the
      index around if it is too large for the vector (like a modulo)
    - Another option is to build in the vector length information to the ark,
        so instead of using fromList2D as-is, use a data structure that knows
        its own length once constructed (like we do for Sentence and others)
    - (*I did both ideas*)

x set style, mode, meter from input file and pass as one structure throughout
x adjust for mode
    x add key signature
    x add ficta/inflected notes
        - We are just taking his sharp or flat scale notes and putting ficta
          on those notes every time. We are not doing any actual ficta
          application by rule. 
x - Fix out-of-range notes: (you are setting notes per voice with knowledge of
   mode, so you should be able to bring in not just an octave per voice
   (voice2octave) but a range and check that the note is in that range.
   Change octave if too high or too low.
x avoid too-large leaps
    - In a long stepwise descent if it goes below range the adjustment
      algorithm will result in a seventh when it just can't keep going down.
    - Ideally you would look at the whole phrase and do some kind of badness
      score to evaluate whether to shift the whole phrase.
    - Or, get into Kircher's recommendations to swap voice parts when they go
      out of range (would also depend on a badness score).

x check transitions between choruses

x read text from file including title, changes of meter or  style

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
x add new rules for determining pinax and column dependent on textMeter
x add ability to change column per order of verse lines
x add sample texts in the necessary meters

# Optimization

- clarify variable names
- expand and improve documentation
- use safe list and vector indexing: (!?) -- needed?
- check user XML input (validate vs DTD)




