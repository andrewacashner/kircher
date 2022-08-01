-- vim: set foldmethod=marker :

-- {{{1 metadata
{-|
Module      : Arca_musarithmica.Syntagma1.Pinax09
Description : Data for the ark, syntagma 1, pinax 9 (Phaleucium Hendecasyllabum)
Copyright   : Data from Kircher, 1650, implementation (c) 2022 Andrew A. Cashner
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark (bk II, p.
93): "Musarithmos Melatheticos Poetics continens pro Phaleucijs
Hendecasyllabis".  This /pinax/ ('s1p9') is for poetry in 11-syllable lines
(with long penultimate).  There are four columns, one for each successive line
(but called "strophes").  There is only one 'Aedifico.RpermTable' for all four
columns.

This pinax only has nine rows of vperms.

What look like the normal three groups of rperms are actually duple major (cut
C), duple minor (compasillo, C), and triple major (cut C 3).  There is
actually no rperm for triple minor (C3).

It may be used with tone I, II, III, IV, and VII.
-}
-- }}}1

module Arca_musarithmica.Syntagma1.Pinax09 (s1p9) where

import Aedifico
    ( Dur (..)
    , buildPinax
    )

-- | Pinax 9
s1p9 = buildPinax [c0, c1, c2, c3] 

-- {{{1 c0
c0 = (c0v, c0r)

-- {{{2 c0v

c0v =     [
        [ -- 0
            [5, 5, 3, 2, 3, 3, 4, 3, 2, 1, 7],
            [7, 7, 8, 7, 1, 1, 4, 5, 5, 6, 7],
            [2, 3, 5, 5, 5, 5, 4, 1, 2, 3, 5],
            [5, 3, 1, 5, 1, 8, 6, 8, 7, 6, 5] -- CN? for 5, 3, 1, 5, 1, 8, *5, 8, 7, 6, 5
        ],
        [ -- 1
            [5, 5, 5, 4, 3, 2, 5, 4, 3, 2, 1],
            [8, 7, 7, 6, 5, 5, 7, 6, 5, 5, 5],
            [3, 2, 3, 8, 8, 7, 7, 8, 8, 7, 8],
            [1, 5, 3, 4, 1, 5, 3, 4, 1, 5, 1]
        ],
        [ -- 2
            [5, 5, 5, 5, 4, 5, 5, 3, 4, 2, 3],
            [8, 8, 8, 8, 6, 5, 5, 6, 6, 5, 5],
            [3, 3, 3, 3, 1, 3, 3, 1, 1, 7, 1],
            [1, 1, 1, 1, 4, 3, 3, 6, 4, 5, 1]
        ],
        [ -- 3
            [5, 5, 5, 5, 5, 5, 4, 4, 3, 2, 2], -- CN for 5, 5, 5, 5, 5, 5, 4, 3, 2, 2, 2
            [1, 1, 1, 1, 7, 1, 1, 2, 1, 6, 7],
            [3, 3, 3, 3, 2, 3, 4, 2, 5, 4, 5],
            [8, 8, 8, 8, 5, 8, 4, 7, 8, 2, 5]
        ],
        [ -- 4
            [5, 5, 5, 3, 5, 5, 4, 2, 1, 2, 3],
            [1, 7, 1, 1, 7, 7, 6, 5, 5, 4, 5],
            [3, 2, 3, 1, 2, 5, 6, 7, 1, 7, 7],
            [1, 5, 8, 6, 5, 3, 4, 5, 6, 7, 3]
        ],
        [ -- 5
            [5, 5, 5, 2, 3, 1, 5, 5, 4, 3, 2],
            [8, 7, 8, 2, 7, 6, 5, 5, 7, 5, 5],
            [3, 2, 3, 2, 7, 4, 2, 3, 2, 1, 7],
            [1, 5, 8, 7, 3, 4, 5, 3, 7, 1, 5]
        ],
        [ -- 6
            [5, 5, 3, 2, 3, 3, 2, 3, 2, 1, 7],
            [7, 7, 1, 7, 1, 1, 7, 5, 5, 6, 7],
            [2, 3, 5, 5, 5, 5, 5, 1, 2, 3, 5],
            [5, 3, 1, 5, 1, 8, 5, 8, 7, 6, 5]
        ],
        [ -- 7
            [5, 5, 5, 4, 3, 2, 5, 4, 3, 2, 1],
            [8, 7, 7, 6, 5, 5, 7, 6, 5, 5, 5],
            [3, 2, 3, 8, 8, 7, 8, 8, 8, 7, 8],
            [1, 5, 3, 4, 1, 5, 3, 4, 1, 5, 1]
        ],
        [ -- 8 
            [5, 5, 5, 2, 3, 1, 5, 5, 4, 3, 2],
            [8, 7, 8, 2, 7, 6, 5, 5, 7, 5, 5],
            [3, 2, 3, 2, 5, 4, 2, 3, 2, 1, 7], -- unclear
            [1, 5, 8, 7, 3, 4, 5, 3, 7, 1, 5]
        ]
    ]
-- }}}2
-- {{{2 c0r

c0r =     [
        [ -- Duple
            [[Sb, Mn, Mn, Mn, Mn, Sb, Mn, Sb, Mn, Sb, Sb]],
            [[Sb, Mn, Mn, Sb, Mn, Sb, Mn, Mn, Mn, Sb, Sb]],
            [[Sb, Mn, Mn, Mn, Mn, SbD, Mn, Mn, Mn, Sb, Sb]],
            [[Sb, Mn, Mn, Sb, Mn, Sb, Mn, MnD, Sm, Sb, Sb]],
            
            [[Mn, Sm, Sm, Mn, Sm, Mn, Sm, Sm, Sm, Mn, Mn]],
            [[Sm, Fs, Fs, Sm, Sm, MnD, Sm, Sm, Sm, Mn, Sb]],
            [[Sm, Fs, Fs, Sm, Sm, Mn, Mn, Sm, Sm, Mn, Sb]],
            [[Sm, Mn, Mn, Mn, Sm, Mn, Mn, Sm, Sm, Mn, Sb]],
            [[Mn, Sb, Sb, Sb, Mn, Sb, Sb, SbD, Mn, Sb, Sb]]
        ],
        [ -- TripleMajor
            [[Sb, Sb, Sb, Br, Sb, Br, Sb, Br, Sb, BrD, BrD]],
            [[Sb, Sb, Sb, Br, Sb, Sb, Sb, Sb, Sb, Br, BrD]],
            [[BrD, Br, Sb, Sb, Sb, Sb, Sb, Sb, Sb, BrD, BrD]]
        ],
        [ -- TripleMinor
        ]
    ]
-- }}}1
-- {{{1 c1
c1 = (c1v, c0r)

-- {{{2 c1v

c1v =     [
        [ -- 0
            [3, 3, 3, 4, 5, 5, 4, 3, 2, 2, 2],
            [7, 7, 7, 7, 7, 5, 7, 5, 5, 6, 7], -- unclear 
            [5, 5, 5, 4, 3, 3, 2, 1, 7, 6, 5],
            [3, 3, 3, 7, 3, 3, 7, 8, 5, 2, 5]
        ],
        [ -- 1
            [3, 2, 5, 3, 3, 1, 5, 3, 3, 2, 3],
            [5, 7, 7, 6, 5, 6, 5, 8, 7, 7, 7],
            [8, 2, 2, 5, 1, 4, 2, 8, 5, 4, 5], -- CN for 8, 2, 2, 5, *5...
            [8, 7, 5, 6, 3, 4, 5, 6, 3, 7, 3]
        ],
        [ -- 2
            [3, 3, 2, 1, 3, 1, 1, 4, 4, 3, 4],
            [5, 5, 5, 6, 7, 6, 5, 7, 7, 5, 4],
            [1, 1, 2, 3, 5, 6, 5, 2, 2, 1, 1],
            [8, 8, 7, 6, 3, 4, 1, 7, 7, 1, 4]
        ],
        [ -- 3
            [3, 3, 3, 3, 2, 3, 3, 2, 2, 1, 7],
            [7, 7, 7, 7, 7, 7, 1, 6, 6, 6, 7],
            [5, 5, 5, 5, 4, 5, 5, 4, 4, 4, 4],
            [3, 3, 3, 3, 7, 3, 1, 2, 4, 4, 7] -- CN for 3, 3, 3, 3, 7, 3, 1, 2, *3, 4, 7
        ],
        [ -- 4
            [5, 5, 4, 3, 2, 3, 2, 4, 3, 2, 2],
            [5, 5, 6, 1, 2, 1, 7, 6, 1, 6, 7],
            [3, 3, 4, 5, 2, 5, 5, 4, 5, 4, 5],
            [3, 3, 2, 1, 7, 1, 5, 2, 1, 2, 5]
        ],
        [ -- 5
            [5, 5, 5, 4, 3, 2, 3, 1, 3, 2, 3],
            [7, 7, 7, 7, 5, 7, 7, 8, 7, 6, 5],
            [3, 3, 3, 2, 3, 4, 5, 3, 7, 4, 7],
            [3, 3, 3, 7, 8, 7, 3, 6, 5, 4, 3]
        ],
        [ -- 6
            [3, 3, 3, 4, 5, 5, 4, 3, 2, 2, 2],
            [7, 7, 7, 7, 7, 5, 7, 5, 5, 6, 7],
            [5, 5, 5, 4, 3, 3, 2, 1, 7, 6, 5],
            [3, 3, 3, 7, 3, 3, 7, 8, 5, 2, 5]
        ],
        [ -- 7
            [3, 2, 5, 3, 3, 1, 5, 3, 3, 2, 3],
            [5, 7, 7, 6, 5, 6, 5, 8, 7, 7, 7],
            [8, 2, 2, 1, 5, 4, 2, 8, 5, 4, 5], -- CN for 8, 2, 2, *5, 5...
            [8, 7, 5, 6, 3, 4, 5, 6, 3, 7, 3]
        ],
        [ -- 8
            [5, 5, 5, 4, 3, 2, 3, 1, 3, 2, 3],
            [7, 7, 7, 7, 5, 7, 7, 8, 7, 6, 5],
            [3, 3, 3, 2, 3, 4, 5, 3, 7, 4, 7],
            [3, 3, 3, 7, 8, 7, 3, 6, 5, 4, 3]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = (c2v, c0r)

-- {{{2 c2v

c2v =     [
        [ -- 0
            [5, 5, 5, 3, 3, 1, 1, 7, 1, 7, 1],
            [7, 7, 7, 1, 7, 6, 5, 5, 4, 5, 5],
            [3, 3, 3, 3, 5, 4, 3, 2, 1, 2, 3],
            [3, 3, 3, 6, 3, 4, 1, 5, 6, 5, 1]
        ],
        [ -- 1  
            [5, 5, 5, 4, 4, 4, 4, 3, 2, 1, 2],
            [7, 7, 7, 7, 7, 6, 4, 5, 7, 6, 7],
            [3, 3, 3, 2, 2, 8, 7, 7, 7, 8, 7],
            [3, 3, 3, 7, 7, 4, 2, 3, 7, 4, 7]
        ],
        [ -- 2
            [6, 6, 6, 6, 5, 6, 5, 3, 2, 2, 3], -- CN! *5, *5, *5, *5, 5, *5, 5, 3, *1, 2, *4
            [8, 8, 8, 8, 7, 8, 7, 8, 7, 6, 5], -- CN! *7, *7, *7, *7, 7, *7, 7, 8, 7, 6, 5
            [3, 3, 3, 3, 3, 3, 3, 3, 7, 4, 7],
            [6, 6, 6, 6, 3, 6, 3, 6, 5, 4, 3]
        ],
        [ -- 3  
            [7, 7, 1, 2, 4, 5, 5, 4, 4, 2, 1],
            [2, 2, 1, 7, 6, 5, 5, 6, 6, 7, 1],
            [4, 4, 4, 2, 6, 7, 1, 1, 4, 2, 3], -- CN 4, 4, *2, 2...
            [7, 7, 6, 5, 4, 3, 1, 4, 4, 5, 1]
        ],
        [ -- 4
            [3, 3, 3, 2, 2, 1, 7, 1, 1, 2, 3],
            [3, 3, 3, 4, 2, 3, 5, 6, 5, 5, 5],
            [5, 5, 5, 2, 2, 3, 5, 4, 3, 2, 1],
            [8, 8, 8, 7, 7, 6, 3, 4, 1, 5, 1]
        ],
        [ -- 5
            [5, 5, 5, 4, 3, 2, 5, 4, 1, 2, 1],
            [8, 8, 8, 7, 5, 7, 7, 6, 6, 7, 8], -- CN 8, 8, 8, 7, 5, 7, 7, 6, *5, 7, 8
            [3, 3, 3, 2, 1, 7, 7, 4, 4, 5, 5],
            [1, 1, 1, 5, 8, 7, 3, 4, 6, 5, 1]
        ],
        [ -- 6
            [5, 5, 7, 3, 3, 1, 1, 7, 1, 7, 1], -- CN 5, 5, *8, 3, 3, 1, 1, 7, 1, 7, 1
            [7, 7, 7, 1, 7, 6, 5, 5, 4, 5, 5],
            [3, 3, 3, 3, 5, 4, 3, 2, 1, 2, 3],
            [3, 3, 5, 6, 3, 4, 1, 5, 6, 5, 1]
        ],
        [ -- 7
            [5, 5, 5, 4, 4, 4, 4, 3, 2, 1, 2],
            [7, 7, 7, 7, 7, 6, 4, 7, 7, 6, 7],
            [3, 3, 3, 2, 2, 2, 7, 3, 7, 8, 5], -- unclear!
            [3, 3, 3, 7, 7, 4, 2, 5, 7, 4, 7] -- unclear
        ],
        [ -- 8
            [5, 5, 5, 2, 3, 2, 5, 4, 1, 2, 1], -- unclear
            [8, 8, 8, 7, 5, 7, 5, 6, 6, 7, 8], -- unclear!
            [3, 5, 1, 5, 1, 7, 7, 4, 4, 5, 5],
            [1, 1, 1, 5, 8, 7, 3, 4, 6, 5, 1]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = (c3v, c0r)

-- {{{2 c3v

c3v =     [
        [ -- 0
            [3, 3, 3, 3, 5, 4, 3, 2, 1, 2, 3],
            [7, 7, 7, 1, 7, 6, 5, 5, 4, 5, 5],
            [5, 5, 5, 3, 3, 1, 1, 7, 1, 7, 1],
            [3, 3, 3, 6, 3, 4, 1, 5, 6, 5, 1]
        ],
        [ -- 1
            [4, 1, 2, 4, 5, 5, 4, 3, 1, 2, 1],
            [7, 8, 7, 4, 5, 3, 4, 5, 6, 7, 8],
            [7, 3, 5, 8, 3, 3, 2, 7, 4, 2, 3],
            [7, 6, 5, 4, 3, 1, 2, 3, 4, 5, 1]
        ],
        [ -- 2
            [5, 5, 5, 5, 4, 3, 3, 2, 1, 7, 1],
            [7, 7, 7, 7, 7, 7, 8, 7, 8, 2, 8], -- CN 7, 7, 7, 7, 7, 7, *7, 7, 8, 2, 8],
            [3, 3, 3, 3, 2, 3, 3, 5, 3, 5, 5],
            [3, 3, 3, 3, 7, 3, 1, 5, 6, 5, 1]
        ],
        [ -- 3
            [3, 3, 2, 1, 7, 6, 5, 4, 5, 5, 5],
            [5, 5, 5, 1, 2, 6, 7, 4, 1, 7, 1],
            [5, 5, 2, 3, 7, 1, 3, 4, 3, 2, 1],
            [8, 8, 7, 6, 5, 4, 3, 2, 1, 5, 1] -- unclear
        ],
        [ -- 4
            [3, 3, 3, 2, 2, 1, 7, 1, 1, 2, 3],
            [5, 5, 5, 7, 7, 8, 5, 6, 5, 5, 5],
            [1, 1, 1, 2, 2, 3, 5, 4, 3, 2, 1],
            [8, 8, 8, 7, 7, 6, 3, 4, 1, 5, 1]
        ],
        [ -- 5
            [5, 5, 5, 4, 3, 2, 5, 4, 1, 2, 1],
            [8, 8, 8, 7, 5, 7, 7, 6, 6, 7, 8], -- CN 8, 8, 8, 7, 5, 7, 7, 6, *5, 7, 8
            [3, 3, 3, 2, 1, 7, 7, 4, 4, 5, 5],
            [1, 1, 1, 5, 8, 7, 3, 4, 6, 5, 1]
        ],
        [ -- 6
            [3, 3, 3, 3, 5, 4, 3, 2, 1, 2, 3],
            [7, 7, 7, 1, 7, 6, 5, 5, 4, 5, 5],
            [5, 5, 5, 3, 3, 1, 1, 7, 1, 7, 1],
            [3, 3, 3, 6, 3, 4, 1, 5, 6, 5, 1]

        ],
        [ -- 7
            [4, 1, 2, 4, 5, 5, 4, 3, 1, 2, 1],
            [7, 8, 7, 4, 5, 3, 4, 5, 6, 7, 8],
            [7, 3, 5, 8, 3, 3, 2, 7, 4, 2, 3],
            [7, 6, 5, 4, 3, 1, 2, 3, 4, 5, 1]
        ],
        [ -- 8 
            [3, 3, 3, 2, 1, 7, 7, 4, 4, 5, 5],
            [8, 8, 8, 7, 8, 7, 7, 6, 6, 7, 8], -- CN 8, 8, 8, 7, 8, 7, 7, 6, *5, 7, 8
            [5, 5, 5, 4, 3, 2, 5, 4, 1, 2, 1],
            [1, 1, 1, 5, 6, 7, 3, 4, 6, 5, 1]
        ]
    ]
-- }}}2
-- }}}1
