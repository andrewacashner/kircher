# `arca`, second version in Scheme (GNU Guile)

Andrew Cashner, March 2019

(This README written October 2020)

After the first implementation in C I realized I was limited by the strictures
of direct memory storage in C arrays, and I was making lots of linked lists,
so I switched to Scheme.

Because of Guile's SXML functionality for manipulating XML trees in a Scheme
representation, and the data-is-code philosophy, I went whole-hog with doing
everything in XML, from the input data and text through output with MEI.

When MEI became too difficult to wrestle with I began a switch to Lilypond
which I don't think I completed.

Along the way I discovered I did need structured data, like I was creating in
rudimentary ways in C, so I adopted the GOOPS object-oriented system, and
again, went in all the way with making everything an object and getting and
setting things everywhere. 

None of this totally worked out but the trials in both languages were
excellent preparation and justification for switching to Haskell.
