# `arca`, first implementation in C

Andrew Cashner, February 2019

(This README written October 2020)

The ark is implemented as a deeply nested array.

Simple functions access the different parts of the ark with necessary
safeguards to avoid indexing past the end of the different-sized sub-arrays.

The text is input with the syllables separated Lilypond style (`syl -- la --
ble`), with each phrase on a new line, and the number of syllables and
penultimate length (`L` or `S`) explicitly notated at the beginning of each
line:

````
6 S Lau -- da -- te Do -- mi -- num
````

Ark numbers for pitches and enums for rhythms are converted to music and then
output in Lilypond format.
I started developiong initial methods for dealing with vocal ranges, overlaps,
mode, and musica ficta.

When I realized I using more and more linked lists and, as they say,
implementing half of LISP badly, I switched to Scheme for the next version.

## To build

`make`

## To test

Run `arca 1 2` for a setting of `text/ps-150.txt` in the first mode, duple
meter. (You may need to manually create the `build` folder first.)
