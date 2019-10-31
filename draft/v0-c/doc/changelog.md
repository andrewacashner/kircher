# ARCA -- Changelog

Andrew A. Cashner

A digital version of Athanasius Kircher's "Arca musarithmica"
for automatic music composition,
from *Musurgia universalis* (Rome, 1650) Bk. 8

- 2019/02/04 
    + Begun, set up data structures and basic algorithms for table lookups,
      using syntagma 1, pinax 1.
- 2019/02/08      
    + Basic operations work, given "cooked" input. Second program, `arca2ly`,
      takes plain text output from `arca` and translates it to Lilypond code to
      produce PDF.
- 2019/02/11      
    + Syntagma 1, pinax 2 input.
- 2019/02/12
    + New input module to read pre-scanned text format
- 2019/02/13
    + Restructured in modules with headers, new makefile
- 2019/02/15
    + Working proof-of-concept. Takes input file with Latin text pre-divided
      into syllables and chunks, and with syllable count and penultimate vowel
      length indicated. Selects the appropriate pinax for the syllables and
      vowel length, randomly selections a voice permutation in the given mode
      and one of the rhythm permutations in the given meter.
      Using linked list for the text input, table lookups for the main
      computation, list of four linked lists for music output (SATB).
      Outputs in Lilypond format. test.sh script produces PDF scores for each
      mode.
        - Problems: Octaves don't make sense (e.g., 2 8 2 8), musica ficta;
          tripla maior rhythms don't all work; various data entry mistakes.


