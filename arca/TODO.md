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
    - add key signature
    - add ficta/inflected notes
x - Fix out-of-range notes: (you are setting notes per voice with knowledge of
   mode, so you should be able to bring in not just an octave per voice
   (voice2octave) but a range and check that the note is in that range.
   Change octave if too high or too low.

x avoid too-large leaps
x check transitions between choruses

x read text from file including title, changes of meter or  style

# Adjustments (confirm we are doing as he recommends)

- avoid out-of-range by swapping voices
- mutation between tonoi/modes (Kircher II: 72 among other places)
- avoid forbidden intervals 
- avoid forbidden parallels
- avoid too-large intervals between voices
- avoid voice crossings
- add musica ficta
- repeating notes for multisyllabic words

# Optimization

- use safe list and vector indexing: (!?) -- needed?
- check user input

# Expanding into more pinakes, syntagmata
- Need a way to indicate user-made groupings vs. leaving it up to machine
  (perhaps as override)

<example>
    <verse poemMeter="Adonius">
        <stanza>
            <l>`Nu-bi-bus `at-ris</l>
            <l>`Con-di-ta, `nul-lum</l>
            <l>`Fun-de-re `pos-sum</l>
            <l>`Sy-de-ra `lu-men</l>
        </stanza>
    </verse>
</example>



