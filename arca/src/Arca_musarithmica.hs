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

import Data.Maybe
    (fromJust)

import Aedifico 
    ( Accid  (..)
    , Arca   (..)
    , Dur    (..)
    , Mode   (..)
    , ModeList
    , ModeSystem
    , Pitch  (..)
    , PinaxLabel (..)
    , PinaxModeList
    , Pnum   (..)
    , Style  (..)
    , System (..)
    , VoiceRanges
    , fromList2D
    , simplePitch
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
    perms      = fromList [s1, s2],
    modes      = _modeList,
    systems    = _modeSystems,
    pinaxModes = _pinaxModes,
    ranges     = _vocalRanges
}

-- | Range for each voice, based on SATB C-clef ranges, up to one ledger line
-- above and below
_vocalRanges :: VoiceRanges
_vocalRanges = map (\(low, high) -> (simplePitch low, simplePitch high))
        [ ( (PCa, 3), (PCf, 5) ) -- Soprano
        , ( (PCd, 3), (PCb, 4) ) -- Alto
        , ( (PCb, 2), (PCg, 5) ) -- Tenor
        , ( (PCe, 2), (PCc, 4) ) -- Bass
        ]

-- | Mode system ('Durus' or 'Mollis') per mode
_modeSystems :: ModeSystem
_modeSystems = fromList [
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

_modeList :: ModeList
_modeList = fromList2D [
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
            (PCb, Na), 
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
        [   -- Mode 4 (Iconismus version; p. 51 is E mode with Bb and G#)
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
            (PCb, Fl), 
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
            -- (C mode, mollis)
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
            (PCb, Fl),  
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
            (PCb, Fl), 
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na)
        ]
    ]

-- ** Appropriate modes for each pinax

-- | Set of all mode labels
_allModes = [ Mode1
            , Mode2
            , Mode3
            , Mode4
            , Mode5
            , Mode6
            , Mode7
            , Mode8
            , Mode9
            , Mode10
            , Mode11
            , Mode12
            ]

-- | Exclude elements of list in arg1 from list in arg2
listExclude :: (Foldable t, Eq a) => t a -> [a] -> [a]
listExclude = filter . flip notElem

-- | Create a list of modes, excluding blacklist from '_allModes'
allModesExcept :: [Mode] -> [Mode]
allModesExcept blacklist = listExclude blacklist _allModes

-- | Modes appropriate for each pinax
_pinaxModes :: PinaxModeList
_pinaxModes = fromList2D 
    [ -- syntagma 1
      [ [allModesExcept [Mode4, Mode5]]
      , [allModesExcept [Mode4, Mode5]]
      , [_allModes]
      , [[Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]]
      , [[Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]]
      , [[Mode5, Mode6, Mode8, Mode12]]
      , [[Mode5, Mode6, Mode8, Mode10, Mode12]]
      , [[Mode5, Mode6, Mode7, Mode8, Mode11, Mode12]]
      , [[Mode1, Mode2, Mode3, Mode4, Mode7]]
      , [[Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]]
      , [[Mode5, Mode6, Mode7, Mode8, Mode11, Mode12]]
      ]
    , -- syntagma 2
      [ [[Mode5, Mode6, Mode7, Mode8, Mode11, Mode12]]
      , [_allModes]
      , [[Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]]
      , [ [Mode5, Mode6, Mode7, Mode8, Mode11, Mode12] 
        , [Mode5, Mode6, Mode7, Mode8, Mode11, Mode12]
        , [Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]
        , [Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]
        ]
      , [[Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]]
      , [[Mode1, Mode2, Mode3, Mode4, Mode9, Mode10]]
      ]
    ]

