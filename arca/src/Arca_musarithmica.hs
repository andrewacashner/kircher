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

module Arca_musarithmica where

import Data.Vector 
    (fromList)

import Data.Maybe
    (fromJust)

import Aedifico 
    ( Accid         (..)
    , Arca          (..)
    , Dur           (..)
    , Tone          (..)
    , ToneList
    , ToneSystem
    , Pitch         (..)
    , PinaxLabel    (..)
    , PinaxToneList
    , Pnum          (..)
    , Style         (..)
    , System        (..)
    , VoiceRange    (..)
    , VoiceRanges   (..)
    , fromList2D
    , simplePitch
    )

import Arca_musarithmica.Syntagma1
    (s1)

import Arca_musarithmica.Syntagma2
    (s2)

-- * Build the ark

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

The arca also includes Kircher's list of notes in each tone with their
accidentals, a list indicating which tones are /cantus durus/ (all naturals in
the signature) or /cantus mollis/ (one B flat in the signature), a list of
which tones are acceptable in each /pinax/, and a list of the acceptable
ranges for each voice based on the most conventional clef combination.
-}
arca :: Arca
arca = Arca {
    perms      = fromList [s1, s2],
    tones      = _toneList,
    systems    = _toneSystems,
    pinaxTones = _pinaxTones,
    ranges     = _vocalRanges
}

-- ** Voice ranges

-- | Range for each voice, based on SATB C-clef ranges, generally up to one
-- note above and below the staff (Soprano C1, alto C3, tenor C4, bass f4
-- clefs), as shown on the front of the ark in Iconismus XIV. We are using the
-- untransposed ranges. These are notably different from those of a modern
-- mixed choir, as Kircher as all-male choirs in mind and the alto clef gives
-- a much lower range.
--
-- NB (unimplemented part of specification):
--
-- Kircher says that if a voice goes out of range, one option to fix it
-- is to switch to the transposing set of clefs shown on the ark. We are not
-- implementing this, because (1) nobody ever switched to transposing clefs
-- for a single phrase and then back, (2) we have a better algorithm for
-- setting the melodies within range than the incomplete specification
-- provided by Kircher (@Cogito.Musarithmia.stepwiseVoiceInRange@).
_vocalRanges :: VoiceRanges
_vocalRanges = VoiceRanges {
    sopranoRange = VoiceRange (simplePitch (PCc, 4)) (simplePitch (PCf, 5)),
    altoRange    = VoiceRange (simplePitch (PCf, 3)) (simplePitch (PCa, 4)),
    tenorRange   = VoiceRange (simplePitch (PCd, 3)) (simplePitch (PCf, 4)),
    bassRange    = VoiceRange (simplePitch (PCf, 2)) (simplePitch (PCb, 3))
}

-- ** Tones

-- | Tone system ('Durus', all naturals; or 'Mollis', one B flat) per tone
_toneSystems :: ToneSystem
_toneSystems = fromList [
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

-- | Notes in the scale for each tone, with accidentals:
--
-- Kircher includes suggested flats and sharps on notes likely to be altered
-- in /musica ficta/ practice; in his tone tables he omits the B flats that
-- would always be added in tones in /cantus mollis/
--
-- We include both here, and elsewhere in the program we determine whether the
-- B flat is from the signature or should be treated as /ficta/.
--
-- The tones on p. 51 and on the Iconismus illustration do not agree. We
-- follow the (presumably later) version on the Iconismus, which corrects
-- errors in the /mensa tonographica/ of p. 51.
_toneList :: ToneList
_toneList = fromList2D [
        [   -- Tone 1
            (PCd, Na), 
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl),
            (PCc, Sh),
            (PCd, Na)
        ],
        [   -- Tone 2 
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl), 
            (PCc, Na),
            (PCd, Na),
            (PCe, Fl),
            (PCf, Sh),
            (PCg, Na)
        ],
        [   -- Tone 3 
            (PCa, Na), 
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Sh),
            (PCa, Na)
        ],
        [   -- Tone 4 (Iconismus version; p. 51 is E tone with Bb and G#!)
            (PCa, Na), 
            (PCb, Na), 
            (PCc, Sh),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na)
        ],
        [   -- Tone 5
            (PCb, Fl),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na), -- should this be Fl?
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl)
        ],
        [   -- Tone 6
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Fl), 
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na)
        ],
        [   -- Tone 7
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Sh),
            (PCg, Na)
        ],
        [   -- Tone 8
            -- for Tone8 using Iconismus XIV not the /two/ different versions
            -- of tone 8 on Bk 2, p. 51 (C tone, durus or mollis)
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Sh),
            (PCg, Na)
        ],
        [   -- Tone 9
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),  -- b^6 on p. 51
            (PCc, Sh),
            (PCd, Na)
        ],
        [   -- Tone 10
            (PCa, Na),
            (PCb, Na),
            (PCc, Na),
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na)
        ],
        [   -- Tone 11
            (PCc, Na), 
            (PCd, Na),
            (PCe, Na),
            (PCf, Na),
            (PCg, Na),
            (PCa, Na),
            (PCb, Na),
            (PCc, Na)
        ],
        [   -- Tone 12
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

-- *** Appropriate tones for each pinax

-- | Set of all tone labels
_allTones :: [Tone]
_allTones = [ Tone1
            , Tone2
            , Tone3
            , Tone4
            , Tone5
            , Tone6
            , Tone7
            , Tone8
            , Tone9
            , Tone10
            , Tone11
            , Tone12
            ]

-- | Exclude elements of list in arg1 from list in arg2
listExclude :: (Foldable t, Eq a) => t a -> [a] -> [a]
listExclude = filter . flip notElem

-- | Create a list of tones, excluding blacklist from @_allTones@
allTonesExcept :: [Tone] -> [Tone]
allTonesExcept blacklist = listExclude blacklist _allTones

-- | Tones appropriate for each pinax
_pinaxTones :: PinaxToneList
_pinaxTones = 
    [ (Simple,
            [ (Pinax1,  [allTonesExcept [Tone4, Tone5]])
            , (Pinax2,  [allTonesExcept [Tone4, Tone5]])
            , (Pinax3a, [_allTones])
            , (Pinax3b, [_allTones])
            , (Pinax4,  [[Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]])
            , (Pinax5,  [[Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]])
            , (Pinax6,  [[Tone5, Tone6, Tone8, Tone12]])
            , (Pinax7,  [[Tone5, Tone6, Tone8, Tone10, Tone12]])
            , (Pinax8,  [[Tone5, Tone6, Tone7, Tone8, Tone11, Tone12]])
            , (Pinax9,  [[Tone1, Tone2, Tone3, Tone4, Tone7]])
            , (Pinax10, [[Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]])
            , (Pinax11, [[Tone5, Tone6, Tone7, Tone8, Tone11, Tone12]])
            ])
    , (Florid,
            [ (Pinax1, [[Tone5, Tone6, Tone7, Tone8, Tone11, Tone12]])
            , (Pinax2, [_allTones])
            , (Pinax3, [[Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]])
            , (Pinax4, [ [Tone5, Tone6, Tone7, Tone8, Tone11, Tone12] 
                       , [Tone5, Tone6, Tone7, Tone8, Tone11, Tone12]
                       , [Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]
                       , [Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]
            ])
            , (Pinax5, [[Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]])
            , (Pinax6, [[Tone1, Tone2, Tone3, Tone4, Tone9, Tone10]])
            ])
    ]

