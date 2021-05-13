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

import Data.Vector 
    (fromList)

import Aedifico 
    (
         Pnum   (..),
         Accid  (..),
         Dur    (..),
         Pitch  (..),
         Mode   (..),
         System (..),
         ModeSystem,
         ModeList,
         VoiceRanges,
         Arca   (..),
         fromList2D
    )

import Arca_musarithmica.Syntagma1
    (s1)

import Arca_musarithmica.Syntagma2
    (s2)

{-| To build the whole ark ('arca'), take the /syntagma/ data entered as a
nested list and convert it to an 'Arca' (which is a vector of vectors down
to the 'Aedifico.Column' level). 

> arca = fromList [s0] :: Arca

= How sub-elements are built

To build the /syntagma/, convert the /pinakes/ from lists to vectors.  s0' has
two /pinakes/: one for long penultimate syllables (@s0p0@) and one for short
(@s0p1@).

> s0 = fromList [s0p0, s0p1] :: Syntagma

We build the each 'Aedifico.Pinax' from 'Aedifico.Column's. 
The first one (@s0p0@) is for phrases with a long penultimate syllable.
There are five columns (@c0@ ... @c4@). All the columns are a 2-tuple of a
'Aedifico.Vperm' and an 'Aedifico.Rperm':

> c0 = (c0v, c0r) :: Column

The first element (@c0v@) is a 'Aedifico.VpermTable' with the pitch numbers:

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
   
The second element (@c0r@) is an 'Aedifico.RpermTable' with matching rhythm
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

arca :: Arca
arca = Arca {
    perms   = fromList [s1, s2],
    modes   = modeList,
    systems = modeSystems,
    ranges  = vocalRanges
}

vocalRanges :: VoiceRanges
vocalRanges = [
        -- Soprano
        (Pitch { pnum = PCb, oct = 3, accid = Na, dur = DurNil },
         Pitch { pnum = PCe, oct = 5, accid = Na, dur = DurNil }),
        -- Alto
        (Pitch { pnum = PCe, oct = 3, accid = Na, dur = DurNil },
         Pitch { pnum = PCa, oct = 4, accid = Na, dur = DurNil }),
        -- Tenor
        (Pitch { pnum = PCc, oct = 3, accid = Na, dur = DurNil },
         Pitch { pnum = PCf, oct = 4, accid = Na, dur = DurNil }),
        -- Bass
        (Pitch { pnum = PCf, oct = 2, accid = Na, dur = DurNil },
         Pitch { pnum = PCb, oct = 3, accid = Na, dur = DurNil })
    ]

modeSystems :: ModeSystem
modeSystems = fromList [
        Durus,
        Mollis,
        Durus,
        Durus,
        Mollis,
        Mollis,
        Durus,
        Durus,
        Mollis,
        Durus,
        Durus,
        Mollis
    ]

modeList :: ModeList
modeList = fromList2D [
        [   -- Mode 1
            (PCd, Na), 
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl),
            (PCc, Sh),
            (PCd, Na)
        ],
        [   -- Mode 2 
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl), -- Na in Iconismus
            (PCc, Na),
            (PCd, Na),
            (PCe, Fl),
            (PCf, Sh),
            (PCg, Na)
        ],
        [   -- Mode 3 
            (PCa, Na), 
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Sh),
            (PCa, Na)
        ],
        [   -- Mode 4
            (PCa, Na), 
            (PCb, Na), 
            (PCc, Sh),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na)
        ],
        [   -- Mode 5
            (PCb, Fl),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl)
        ],
        [   -- Mode 6
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl), -- Na in Iconismus
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na)
        ],
        [   -- Mode 7
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Sh),
            (PCg, Na)
        ],
        [   -- Mode 8
            -- for Mode8 using Iconismus XIV not different version on Bk 2, p. 52
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Sh),
            (PCg, Na)
        ],
        [   -- Mode 9
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl),  -- Na in Iconismus
            (PCc, Sh),
            (PCd, Na)
        ],
        [   -- Mode 10
            (PCa, Na),
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na)
        ],
        [   -- Mode 11
            (PCc, Na), 
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),
            (PCc, Na)
        ],
        [   -- Mode 12
            (PCf, Na), 
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl), -- Na in Iconismus
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na)
        ]
    ]


