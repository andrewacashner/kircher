---
title:  A Computer for Musical Composition from the Seventeenth Century, 
author: 'Andrew A. Cashner, <andrew.cashner@rochester.edu>'
...

# Kircher's Ark

As described in Kircher's *Musurgia universalis* (Rome, 1650), book 8, 
the ark is a box containing rods (*pinakes*), each of which includes columns
with voice and rhythm permutations. The rods are grouped according to style
into *syntagmata*, where *syntagma* 1 is simple homorhythmic counterpoint.
There are two surviving exemplars of physical implementations of the ark.

The top part of Kircher's "rods" contain tables table of numbers with four
rows, where the numbers represent pitch offsets from a modal base note, and
the rows are the notes for the four voice parts SATB.  Each table represents
the notes to set a single phrase of text with a given number of syllables.

## Using Kircher's Ark

Kircher's system works like this: 

1. Analyze and prepare the text.

    - The user chooses a text and an appropriate mode and style.
    - The user segments the text into phrases according to its poetic
      structure and meter.
    - For each phrase, the user counts the syllables and notes whether the
      penultimate phrase is short or long.

2. Access the appropriate data from the ark to set each phrase of text to music.

    - The style determines which *syntagma* to pull from; the penultimate
      length determines which *pinax* to select from that *syntagma*; the
      number of syllables determines which column to select from the *pinax*.
    - The user freely chooses one of the voice permutations.
    - The user chooses the appropriate musical meter for the text and freely
      chooses one of the rhythmic permutations for that meter, to match the
      voice permutations.

3. Convert the data into musical notes and write down the result.

    - The user prepares music paper with staves and an appropriate combination
      of clefs for the four voices.
    - The user takes the numbers from the voice permutation and converts them
      to pitches by adding the number to the base note determined by the mode,
      using the ark's mode table. For example, in mode 1, starting on D, a 2
      in the table would become an E on the music. The user selects the
      correct octave for these pitches based on what fits reasonably on the
      staff with the given clefs (this means the clefs effectively determine
      the ambitus for each voice part). 
    - The user combines the pitches with the note values from the chosen
      rhythm permutation and then writes down the notes on the music.
    - After this the user must check for problems like leaps over too large of
      an interval, parallel fifths or octaves, and too large or small distance
      between voice parts. Depending on the mode the user must also add
      *musica ficta*, which is partly specified by the mode table and partly
      according to traditional rules.
    - Repeat this process for every phrase of text and you have a complete
      vocal composition in four voices.

# Implementation in Haskell

## Modules

### `Arca` module: The ark data

We set up the pitches, accidentals, voice names, and durations as data types
intelligible to the programmer, using enumerable types. We then establish
types to structure the data in a way analogous in the pattern of storage and
means of access to the way Kircher does. This means a deeply nested vector of
vectors for the ark, *syntagmata*, and *pinakes*; each pinax contains a vector
of columns.  Each column contains a pair of two elements: a vector of the
voice permutations and a vector of the rhythm permutations. 

The voice permutations are basically 2-dimensional arrays with pitch numbers
for the four voices, implemented as a vector of vectors. 
The rhythm permutations are also two-dimensional but in this case one
dimension is the type of meter and the other dimension is the rhythmic values
to match with one voice permutation.

We set up the means to build the whole data structure from the input data in
the `Arca/` directory, and we set up methods to access each part of the data
from that structure. 

### `Lectio` module: Preparing the text

### `Cogito` module: Choosing the data and converting it to music

### `Scribo` module: Writing it down


## Separating Deterministic and Free Elements


# TODO

# Notes

- *Note bene* the last stanza of *Iste confessor Domini*, one of Kircher's
  paradigm texts (?):

| Sit salus illi, decus atque virtus,
| qui supra caeli residens caecumen,
| **totius mundi machinam gubernat**
| trius et unus. Amen.

