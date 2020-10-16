{-|
Module      : Arca_musarithmica
Description : Builds Kircher's Ark
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

/Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./

This module builds Kircher's ark as a Haskell data structure, using the types
and methods defined in the @Aedifico@ module (see that module for detailed
description of the ark's structure and implementation.)

Like Kircher's physical implementation, this module is the container for the
data grouped by /syntagma/, each of which holds a group of /pinakes/ (rods
with data tables).

So far we have implemented only Syntagma 1, simple note-against-note
counterpoint. 

-}

module Arca_musarithmica (arca) where

import Data.Vector (fromList)
import Aedifico 
import Arca_musarithmica.Syntagma0 (s0)

{-| To build the whole ark ('arca'), take the /syntagma/ data entered as a
nested list and convert it to an 'Arca' (which is a vector of vectors down
to the 'Column' level). 

> arca = fromList [s0] :: Arca

= How sub-elements are built

To build the /syntagma/, convert the /pinakes/ from lists to vectors.  s0' has
two /pinakes/: one for long penultimate syllables (@s0p0@) and one for short
(@s0p1@).

> s0 = fromList [s0p0, s0p1] :: Syntagma

We build the each 'Pinax' from 'Column's. 
The first one (@s0p0@) is for phrases with a long penultimate syllable.
There are five columns (@c0@ ... @c4@). All the columns are a 2-tuple of a
'Vperm' and an 'Rperm':

> c0 = (c0v, c0r) :: Column

The first element (@c0v@) is a 'VpermTable' with the pitch numbers:

> c0v :: VpermTable
> c0v = fromList2D
>     [
>         [ -- 0
>             [5, 5],
>             [7, 8],
>             [2, 3],
>             [5, 1]
>         ],
>         [ -- 1
>             [5, 5],
>             [7, 7],
>             [2, 2],
>             [5, 5]
>         ],
>         ...
>     ]
   
The second element (@c0r@) is an 'RpermTable' with matching rhythm
permutations in the three metrical categories:

> c0r :: RpermTable
> c0r = fromList2D
>     [
>         [ -- duple
>             [Sb, Sb],
>             [Mn, Mn],
>             [Sm, Sm],
>             [Fs, Fs],
>             [SbD, Mn],
>             [MnD, Sm],
>             [SmD, Fs]
>         ],
>         [ -- triple major
>             [Br, Sb],
>             [BrD, BrD]
>         ],
>         [ -- triple minor
>             [Sb, Mn]
>         ]
>     ]

The other columns are constructed similarly with the data from Kircher.
-}

arca = fromList [s0] :: Arca


