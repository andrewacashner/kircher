{-|
Module      : Arca_musarithmica.Syntagma1.Pinax01
Description : Data for the ark, syntagma 1, pinax 1 (Prose, long penultimate)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s1p1') is for phrases with long penultimate syllables. 
It contains five columns.
-}

module Arca_musarithmica.Syntagma1.Pinax01 (s1p1) where

import Aedifico
    ( Dur (..)
    , buildPinax
    )

-- * 'Pinax': Build from columns

-- | Pinax 1 ('s1p1'): We build the 'Pinax' from 'Column's. 
--
-- "Pinax I. Voces Polysyllabae quae penultimam Longam habent."
--
-- This one is for phrases with a long penultimate syllable.
-- There are five columns (@c0@ ... @c4@). All the columns are a 2-tuple of a
-- 'Aedifico.Vperm' and an 'Aedifico.Rperm'.
s1p1 = buildPinax [c0, c1, c2, c3, c4]

-- ** 'Column'
c0 = (c0v, c0r)

-- *** 'Vperm' (Voice permutations)
c0v = [
        [ -- 0
            [5, 5],
            [7, 8],
            [2, 3],
            [5, 1]
        ],
        [ -- 1
            [5, 5],
            [7, 7],
            [2, 2],
            [5, 5]
        ],
        [ -- 2
            [5, 5],
            [8, 8],
            [3, 3],
            [8, 8]
        ],
        [ -- 3
            [4, 4],
            [6, 6],
            [8, 8],
            [4, 4]
        ],
        [ -- 4
            [4, 3],
            [8, 8],
            [6, 5],
            [4, 8]
        ],
        [ -- 5
            [3, 2],
            [8, 7],
            [5, 5],
            [8, 5]
        ],
        [ -- 6
            [5, 5],
            [8, 7],
            [3, 2],
            [8, 5]
        ],
        [ -- 7
            [5, 5],
            [7, 8],
            [2, 3],
            [5, 1] 
        ],
        [ -- 8
            [2, 3],
            [7, 8],
            [5, 5],
            [5, 1]
        ],
        [ -- 9
            [6, 5],
            [8, 8],
            [4, 3],
            [4, 1]
        ]
    ] 

-- *** Rperms (rhythm permutations)
c0r = [
        [ -- duple
            [[Sb, Sb]],
            [[Mn, Mn]],
            [[Sm, Sm]],
            [[Fs, Fs]],
            [[SbD, Mn]],
            [[MnD, Sm]],
            [[SmD, Fs]]
        ],
        [ -- triple major
            [[Br, Sb]],
            [[BrD, BrD]]
        ],
        [ -- triple minor
            [[Sb, Mn]]
        ]
    ]

-- ** Col 1 (3 syllables)
c1 = (c1v, c1r)

-- *** vperms
c1v = [ 
         [ -- 0
            [3, 2, 3],
            [8, 7, 8],
            [5, 5, 5],
            [1, 5, 1]
        ],
        [ -- 1
            [4, 2, 8],
            [8, 7, 6],
            [6, 5, 3],
            [4, 5, 6]
        ],
        [ -- 2
            [4, 3, 4],
            [8, 8, 8],
            [6, 5, 5],
            [4, 1, 4]
        ],
        [ -- 3
            [2, 2, 2],
            [7, 6, 7],
            [5, 4, 5],
            [5, 2, 5]
        ],
        [ -- 4
            [6, 5, 5],
            [8, 7, 7],
            [4, 2, 2],
            [4, 5, 1]
        ],
        [ -- 5
            [5, 4, 3],
            [8, 8, 8],
            [3, 6, 5],
            [8, 4, 8]
        ],
        [ -- 6
            [5, 5, 5],
            [8, 7, 8],
            [3, 2, 3],
            [1, 5, 1]
        ],
        [ -- 7
            [6, 5, 6],
            [8, 8, 8],
            [4, 3, 4],
            [4, 8, 4]
        ],
        [ -- 8
            [3, 2, 3],
            [8, 7, 8],
            [5, 5, 5],
            [1, 5, 1]
        ],
        [ -- 9
            [6, 5, 5],
            [8, 7, 8],
            [4, 2, 3],
            [4, 5, 1]
        ]
    ]

-- *** rperms
c1r = [
        [ -- duple
            [[Sb, Sb, Sb]],
            [[Mn, Sb, Mn]],
            [[SbR, SmR, Sm, Sb, Sb]],
            [[MnR, Mn, SbD, Mn]],
            [[Sb, Mn, Mn]],
            [[SmR, Sm, Sm, Sm]],
            [[Sm, Mn, Sm]]
        ],
        [ -- triple major
            [[Sb, Sb, Sb]],
            [[SbR, Sb, Br, Sb]]
        ],
        [ -- triple minor
            [[Mn, Mn, Mn]],
            [[SbR, Mn, Sb, Mn]]
        ]
    ]

-- ** Col 2 (4 syllables)
c2 = (c2v, c2r)

-- *** vperms
c2v = [ 
        [ -- 0
            [5, 5, 5, 5],
            [7, 8, 7, 8],
            [2, 3, 2, 3],
            [5, 8, 5, 1]
        ],
        [ -- 1
            [3, 3, 2, 3],
            [8, 8, 7, 8],
            [5, 5, 5, 5],
            [8, 8, 5, 1]
        ],
        [ -- 2
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [3, 4, 5, 1]
        ],
        [ -- 3
            [5, 6, 5, 5],
            [8, 8, 7, 8],
            [3, 4, 2, 3],
            [8, 4, 5, 1]
        ],
        [ -- 4
            [6, 5, 6, 5],
            [8, 8, 8, 8],
            [4, 3, 4, 3],
            [4, 8, 4, 8]
        ],
        [ -- 5
            [2, 3, 2, 3],
            [7, 8, 7, 8],
            [5, 5, 5, 5],
            [5, 1, 5, 1]
        ],
        [ -- 6
            [3, 3, 2, 3],
            [8, 8, 7, 8],
            [5, 5, 5, 5],
            [8, 8, 5, 8]
        ],
        [ -- 7
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [3, 4, 5, 1]
        ],
        [ -- 8
            [2, 3, 2, 3],
            [7, 8, 7, 8],
            [5, 5, 5, 5],
            [5, 8, 5, 1]
        ],
        [ -- 9
            [4, 5, 6, 5],
            [8, 8, 8, 8],
            [6, 5, 4, 3],
            [4, 3, 4, 1]
        ]
    ]

-- *** rperms
c2r = [ 
        [ -- duple
            [[Sb, Sb, Sb, Sb]],
            [[SbD, Mn, Sb, Sb]],
            [[Mn, Mn, Sb, Sb]],
            [[Sm, Sm, Sb, Mn]],
            [[MnR, Sb, Mn, Sb, Sb]],
            [[SmR, Mn, Sm, Mn, Mn]],
            [[Mn, Mn, Mn, Mn]]
        ],
        [ -- triple major
            [[Br, Sb, Br, Sb]],
            [[Br, Sb, BrD, BrD]]
        ],
        [ -- triple minor
            [[Sb, Mn, Sb, Mn]],
            [[Sb, Mn, SbD, SbD]]
        ]
    ]

-- ** Col 3 (5 syllables)
c3 = (c3v, c3r)

-- *** vperms
c3v = [ 
        [ -- 0
            [2, 3, 3, 2, 3],
            [7, 8, 8, 7, 8],
            [5, 5, 5, 5, 5],
            [5, 3, 1, 5, 1]
        ],
        [ -- 1
             -- XXX cn: last s note in kircher is 5
            [5, 6, 6, 5, 6], 
            [7, 8, 8, 8, 8], 
            [3, 3, 4, 3, 4],
            [3, 6, 4, 8, 4]
        ],
        [ -- 2
            [3, 4, 3, 2, 3],
             -- XXX cn: penultimate a note in kircher is 8
            [8, 8, 8, 7, 8], 
            [5, 6, 5, 5, 5],
            [8, 4, 8, 5, 1]
        ],
        [ -- 3
            [2, 8, 2, 7, 8],
            [6, 5, 6, 5, 5],
            [4, 3, 2, 2, 3],
            [2, 3, 4, 5, 1]
        ],
        [ -- 4
            [5, 6, 5, 4, 3],
            [8, 8, 8, 8, 8],
            [3, 4, 5, 6, 5],
            [8, 4, 3, 4, 1]
        ],
        [ -- 5
            [2, 3, 3, 2, 3],
            [7, 8, 8, 7, 8],
            [5, 5, 5, 5, 5],
            [5, 3, 1, 5, 1]
        ],
        [ -- 6
            [4, 3, 2, 7, 8],
            [2, 8, 6, 5, 5],
            [6, 5, 4, 2, 3],
            [2, 3, 4, 5, 1]
        ],
        [ -- 7
            [5, 6, 5, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 4, 3, 2, 3],
            [1, 4, 5, 5, 1]
        ],
        [ -- 8
            [3, 4, 4, 3, 4],
            [8, 8, 8, 8, 8], 
            [5, 6, 6, 5, 6],
            [8, 6, 4, 1, 4]
        ],
        [ -- 9
            [5, 6, 5, 4, 3],
            [8, 8, 8, 8, 8],
            [3, 4, 5, 6, 5],
            [1, 4, 3, 4, 1]
        ]
    ]
-- *** rperms
c3r = [ 
        [ -- duple
            [[Sb, Mn, Mn, Sb, Sb]],
            [[Mn, Sb, Mn, Sb, Sb]],
            [[Sm, Fs, Fs, Sm, Sm]],
            [[Sm, Mn, Sm, Sb, Sb]],
            [[Mn, Sm, Sm, Mn, Mn]],
            [[SmR, Sm, Sm, Sm, Mn, Mn]],
            [[Sb, MnD, Sm, Mn, Mn]],
            [[MnR, Sb, Mn, Mn, Mn, Sb]]
        ],
        [ -- triple major
            [[Sb, Sb, Sb, Br, Sb]],
            [[BrR, Sb, Br, Sb, Br, Sb]]
        ],
        [ -- triple minor
            [[Mn, Mn, Mn, Sb, Mn]],
            [[SbR, Mn, Sb, Mn, SbD, SbD]]
        ]
    ]

-- ** Col 4 (6 syllables)
c4 = (c4v, c4r)

-- *** vperms
c4v = [ 
        [ -- 0
            [5, 5, 5,  3, 5, 5], 
            [8, 8, 7,  8, 7, 8],
            [3, 3, 2,  2, 2, 3], 
            [8, 8, 5,  6, 5, 1]
        ],
        [ -- 1
            [4, 4, 3,  2, 2, 2],
            [2, 2, 8,  7, 6, 7],
            [6, 6, 5,  5, 4, 5],
            [2, 2, 3,  5, 2, 5]
        ],
        [ -- 2
            [3, 2, 3,  5, 4, 5],
            [8, 7, 8,  2, 8, 2],
            [5, 5, 5,  2, 6, 7],
            [8, 5, 8,  7, 6, 5]
        ],
        [ -- 3
            [8, 8, 2,  3, 2, 3],
            [6, 5, 6,  8, 7, 8],
            [4, 5, 4,  5, 5, 5],
            [4, 3, 2,  1, 5, 1]
        ],
        [ -- 4
            [6, 5, 6,  5, 4, 3],
            [8, 8, 8,  8, 8, 8],
            [4, 3, 4,  5, 6, 5],
            [4, 8, 4,  3, 4, 1]
        ],
        [ -- 5
            [3, 8, 7,  8, 7, 8],
            [5, 5, 5,  3, 5, 5],
            [3, 3, 2,  8, 2, 3],
            [8, 8, 5,  6, 5, 1]
        ],
        [ -- 6
            [4, 4, 3,  2, 2, 2],
            [2, 2, 8,  7, 6, 7],
            [6, 6, 5,  5, 4, 5],
            [2, 2, 3,  5, 2, 5]
        ],
        [ -- 7
            [3, 2, 3,  5, 4, 5],
            [8, 7, 8,  2, 8, 2],
            [5, 5, 5,  2, 6, 7],
            [8, 5, 8,  7, 6, 5]
        ],
        [ -- 8
            [6, 4, 3,  2, 2, 3],
            [8, 2, 8,  8, 7, 8],
            [4, 6, 5,  6, 5, 5],
            [4, 2, 3,  4, 5, 1]
        ],
        [ -- 9
            [6, 5, 6,  5, 4, 3],
            [8, 8, 8,  8, 8, 8],
            [4, 3, 4,  5, 6, 5],
            [4, 8, 4,  3, 4, 1]
        ]
    ]

-- *** rperms
c4r = [ 
        [ -- duple
            [[Mn, Mn, Mn, Mn, Sb, Sb]],
            [[Sm, Sm, Sm, Sm, Mn, Mn]],
            [[SbD, Mn, Mn, Mn, Sb, Sb]],
            [[SmR, Mn, Sm, Mn, Mn, Mn, Mn]],
            [[MnR, Sb, Mn, Mn, Mn, Sb, Sb]],
            [[Sm, Sm, SmD, Fs, Mn, Mn]],
            [[MnD, Sm, Sm, Sm, Mn, Sb]]
        ],
        [ -- triple major
            [[Br, Sb, Br, Sb, BrD, BrD]],
            [[SbR, Sb, Sb, Br, Sb, Br, Sb]],
            [[Sb, Sb, Sb, Sb, Br, BrD]]
        ],
        [ -- triple minor
            [[Sb, Mn, Sb, Mn, SbD, SbD]],
            [[MnR, Mn, Mn, Sb, Mn, SbD, SbD]]
        ]
    ]

