-- vim: set foldmethod=marker :

-- {{{1 metadata
{-|
Module      : Arca_musarithmica.Syntagma1.Pinax11
Description : Data for the ark, syntagma 1, pinax 11 (Dodecasyllabicum)
Copyright   : Data from Kircher, 1650, implementation (c) 2022 Andrew A. Cashner
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark (bk II, p.
97): "Dodecasyllaba penultima breuia".  This /pinax/ ('s1p11') is for poetry
in quatrains with 11-syllable lines with short penultimates.  There are four
columns, one for each successive line (but called "strophes").  There is only
one 'Aedifico.RpermTable' for all four columns.

It may be used with tone V, VI, VII, VIII, XI, and XII.
-}
-- }}}1

module Arca_musarithmica.Syntagma1.Pinax11 (s1p11) where

import Aedifico
    ( Dur (..)
    , buildPinax
    )

-- | Pinax 11
s1p11 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = (c0v, c0r)

-- {{{2 c0v

c0v =     [
        [ -- 0
            [3, 3, 3, 2, 2, 3, 3, 2, 4, 5, 5, 5],
            [8, 8, 8, 7, 7, 8, 8, 2, 8, 2, 2, 3],
            [5, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 8],
            [8, 8, 6, 5, 5, 1, 8, 7, 6, 5, 5, 1]
        ],
        [ -- 1
            [3, 4, 4, 5, 5, 6, 5, 5, 3, 2, 2, 3],
            [8, 8, 8, 8, 8, 8, 7, 8, 8, 7, 7, 8],
            [5, 6, 6, 3, 3, 4, 2, 3, 5, 5, 5, 5],
            [8, 6, 4, 8, 8, 4, 5, 3, 1, 5, 5, 1]
        ],
        [ -- 2
            [3, 4, 3, 2, 2, 3, 3, 2, 8, 8, 7, 8],
            [8, 8, 8, 7, 7, 8, 8, 6, 5, 6, 5, 5],
            [5, 6, 5, 5, 5, 5, 5, 4, 3, 2, 2, 3],
            [8, 4, 8, 5, 5, 1, 1, 2, 3, 4, 5, 1]
        ],
        [ -- 3
            [3, 2, 8, 8, 3, 2, 4, 3, 2, 7, 7, 8],
            [8, 7, 6, 6, 8, 7, 2, 8, 6, 5, 5, 5],
            [5, 5, 3, 4, 5, 5, 6, 5, 4, 2, 2, 3],
            [8, 5, 6, 4, 1, 5, 2, 3, 4, 5, 5, 1]
        ],
        [ -- 4
            [8, 2, 2, 8, 8, 2, 8, 8, 3, 2, 2, 3],
            [6, 6, 6, 6, 6, 6, 6, 6, 8, 7, 7, 8],
            [3, 4, 4, 3, 3, 4, 4, 4, 5, 5, 5, 5],
            [6, 4, 2, 6, 6, 2, 4, 4, 1, 5, 5, 1]
        ],
        [ -- 5
            [5, 6, 5, 4, 4, 3, 3, 4, 3, 2, 2, 3],
            [8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 8],
            [3, 4, 5, 6, 6, 5, 5, 6, 5, 5, 5, 5],
            [8, 4, 3, 4, 4, 1, 1, 4, 1, 5, 5, 1]
        ],
        [ -- 6
            [3, 5, 4, 3, 3, 4, 4, 4, 3, 2, 2, 3],
            [8, 2, 2, 8, 8, 2, 8, 8, 8, 7, 7, 8],
            [6, 7, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5],
            [6, 5, 2, 6, 6, 2, 4, 4, 1, 5, 5, 1]
        ],
        [ -- 7
            [3, 2, 8, 8, 3, 2, 4, 3, 2, 2, 2, 3],
            [8, 7, 6, 6, 8, 7, 2, 8, 8, 7, 7, 8],
            [5, 5, 3, 4, 5, 5, 6, 5, 6, 5, 5, 5],
            [8, 5, 6, 4, 1, 5, 2, 3, 4, 5, 5, 1]
        ],
        [ -- 8 
            [3, 5, 4, 3, 3, 4, 4, 4, 3, 2, 2, 3],
            [8, 2, 2, 8, 8, 2, 8, 8, 8, 7, 7, 8],
            [6, 7, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5],
            [6, 5, 2, 6, 6, 2, 4, 4, 1, 5, 5, 1]
        ],
        [ -- 9
            [3, 5, 4, 5, 5, 3, 3, 3, 3, 2, 2, 3],
            [8, 2, 8, 2, 2, 8, 8, 8, 8, 7, 7, 8],
            [5, 5, 6, 7, 7, 5, 5, 5, 5, 5, 5, 5],
            [8, 7, 6, 5, 5, 8, 1, 1, 1, 5, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r

c0r =     [
        [ -- Duple
            [[Sb, Mn, Mn, MnD, Sm, Br, MnD, Fs, Fs, MnD, Sm, Sb]],
            [[Mn, Sb, Mn, MnD, Sm, Sb, Mn, Sm, Sm, SmD, Fs, Mn]],
            [[Mn, Sm, Sm, SmD, Fs, Mn, Sm, Fs, Fs, SmD, Fs, Sb]],
            [[SbD, Sm, Sm, MnD, Sm, Sb, SbD, Sm, Sm, MnD, Sm, Sb]],
            [[MnD, Fs, Fs, SmD, Fs, Mn, Sm, Fs, Fs, SmD, Fs, Sb]],
            [[MnR, Mn, Mn, Mn, MnD, Sm, Mn, MnD, Sm, Mn, MnD, Sm, Sb]],
            [[SmR, Sm, Sm, Sm, SmD, Fs, Sm, SmD, Fs, Sm, SmD, Fs, Sb]]
        ],
        [ -- TripleMajor
            [[Sb, Sb, Sb, SbD, Mn, Sb, SbD, Mn, Sb, SbD, Mn, Sb]], -- last SbD dot missing in col1
            [[SbR, SbR, Sb, Br, Sb, SbD, Mn, Sb, Sb, Sb, Sb, Sb, Sb, Sb]]
        ],
        [ -- TripleMinor
            [[Mn, Mn, Mn, MnD, Sm, Mn, MnD, Sm, Mn, MnD, Sm, Mn]],
            [[SbR, Mn, Sb, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Mn]]
        ]
    ]
-- }}}1
-- {{{1 c1
c1 = (c1v, c0r)

-- {{{2 c1v

c1v =     [
        [ -- 0
            [3, 4, 3, 2, 2, 3, 2, 3, 2, 2, 2, 2], -- CN 3, *2, 3...
            [8, 8, 8, 7, 7, 8, 7, 8, 7, 6, 6, 7],
            [5, 6, 5, 5, 5, 5, 5, 5, 5, 4, 4, 5],
            [8, 4, 8, 5, 5, 8, 5, 1, 5, 2, 2, 5]
        ],
        [ -- 1
            [3, 3, 2, 3, 3, 3, 4, 4, 4, 4, 3, 4],
            [8, 8, 7, 8, 8, 8, 2, 2, 2, 2, 8, 8], -- CN 8, 8, 7, 8, 8, *2...
            [5, 5, 5, 5, 5, 5, 2, 2, 2, 2, 5, 6],
            [8, 8, 5, 8, 8, 8, 7, 7, 7, 7, 8, 4]
        ],
        [ -- 2
            [3, 2, 8, 8, 3, 2, 4, 3, 2, 7, 7, 8],
            [8, 7, 6, 6, 8, 7, 2, 8, 6, 5, 5, 5],
            [5, 5, 3, 4, 5, 5, 6, 5, 4, 2, 2, 3],
            [8, 5, 6, 4, 1, 5, 2, 3, 4, 5, 5, 1]
        ],
        [ -- 3
            [4, 3, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2],
            [2, 8, 8, 7, 7, 8, 7, 6, 7, 6, 6, 7],
            [6, 5, 6, 5, 5, 5, 5, 4, 5, 4, 4, 5],
            [2, 3, 4, 5, 5, 1, 5, 2, 5, 2, 2, 5]
        ],
        [ -- 4
            [3, 3, 2, 3, 3, 4, 4, 4, 3, 2, 2, 2],
            [8, 8, 7, 8, 8, 8, 8, 8, 8, 6, 6, 7],
            [5, 5, 5, 5, 5, 6, 6, 6, 5, 4, 4, 5],
            [8, 8, 5, 8, 8, 4, 4, 4, 8, 2, 2, 5]
        ],
        [ -- 5
            [3, 3, 2, 3, 3, 3, 4, 4, 4, 4, 3, 4],
            [8, 8, 7, 8, 8, 8, 2, 2, 2, 2, 8, 8],
            [5, 5, 5, 5, 5, 5, 2, 2, 2, 2, 5, 6],
            [8, 8, 5, 8, 8, 8, 7, 7, 7, 7, 8, 4]
        ],
        [ -- 6
            [3, 3, 2, 3, 3, 4, 5, 3, 3, 2, 2, 2],
            [8, 8, 7, 8, 8, 8, 7, 8, 8, 6, 6, 7],
            [5, 5, 5, 5, 5, 6, 5, 5, 5, 4, 4, 5],
            [8, 8, 5, 8, 8, 6, 3, 8, 8, 2, 2, 5]
        ],
        [ -- 7
            [4, 3, 2, 7, 7, 8, 7, 6, 7, 6, 6, 7],
            [2, 8, 6, 5, 5, 5, 5, 4, 5, 4, 4, 5],
            [6, 5, 4, 2, 2, 3, 2, 2, 2, 2, 2, 2],
            [2, 3, 4, 5, 5, 1, 5, 2, 5, 2, 2, 5]
        ],
        [ -- 8
            [3, 3, 2, 3, 3, 4, 5, 3, 3, 2, 2, 2],
            [8, 8, 7, 8, 8, 8, 7, 8, 8, 6, 6, 7],
            [5, 5, 5, 5, 5, 6, 5, 5, 5, 4, 4, 5],
            [8, 8, 5, 8, 8, 6, 3, 8, 8, 2, 2, 5] -- unclear
        ],
        [ -- 9
            [4, 4, 3, 2, 2, 3, 3, 5, 5, 5, 4, 5],
            [8, 8, 8, 7, 7, 8, 8, 2, 2, 8, 8, 2],
            [6, 6, 5, 5, 5, 5, 5, 5, 2, 3, 6, 7],
            [4, 4, 1, 5, 5, 1, 8, 7, 7, 6, 6, 5]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = (c2v, c0r)

-- {{{2 c2v

c2v =     [
        [ -- 0
            [2, 2, 2, 2, 2, 2, 4, 4, 3, 2, 2, 3],
            [7, 7, 7, 7, 7, 7, 8, 8, 8, 7, 7, 8],
            [5, 5, 5, 5, 5, 5, 6, 6, 5, 5, 5, 5],
            [5, 5, 5, 5, 5, 5, 4, 4, 1, 5, 5, 1]
        ],
        [ -- 1  
            [3, 3, 5, 4, 3, 4, 2, 3, 2, 2, 2, 3],
            [8, 8, 2, 8, 7, 8, 7, 8, 8, 7, 7, 8],
            [5, 5, 5, 6, 7, 6, 5, 5, 6, 5, 5, 5],
            [8, 8, 7, 6, 5, 4, 5, 3, 4, 5, 5, 1]
        ],
        [ -- 2
            [6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 4, 5],
            [8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 6, 7],
            [4, 4, 4, 4, 3, 4, 2, 2, 2, 2, 2, 2],
            [4, 4, 4, 4, 8, 4, 5, 5, 5, 5, 2, 5]
        ],
        [ -- 3  
            [6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 4, 5],
            [8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 6, 7],
            [4, 4, 4, 4, 3, 4, 2, 2, 2, 2, 2, 2],
            [4, 4, 4, 4, 8, 4, 5, 5, 5, 5, 2, 5]
        ],
        [ -- 4
            [6, 6, 5, 6, 6, 4, 4, 4, 4, 3, 3, 4],
            [8, 8, 8, 8, 8, 8, 8, 2, 2, 8, 8, 8],
            [4, 4, 3, 4, 4, 6, 6, 5, 5, 5, 5, 6],
            [4, 4, 1, 4, 4, 4, 6, 7, 7, 8, 8, 4]
        ],
        [ -- 5
            [3, 3, 5, 4, 3, 4, 2, 3, 2, 2, 2, 3],
            [8, 8, 2, 8, 7, 8, 7, 7, 8, 7, 7, 8],
            [5, 5, 5, 6, 7, 6, 5, 5, 6, 5, 5, 5],
            [8, 8, 7, 6, 5, 4, 5, 3, 4, 5, 5, 1]
        ],
        [ -- 6
            [3, 3, 3, 5, 3, 2, 3, 3, 2, 2, 2, 3],
            [8, 8, 8, 2, 8, 7, 8, 8, 8, 7, 7, 8],
            [5, 5, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5],
            [8, 8, 8, 7, 8, 5, 3, 3, 4, 5, 5, 1]
        ],
        [ -- 7
            [4, 4, 3, 4, 4, 4, 4, 4, 4, 3, 3, 4],
            [8, 8, 8, 8, 8, 8, 8, 2, 2, 8, 8, 8],
            [6, 6, 5, 6, 6, 6, 6, 5, 5, 5, 5, 6],
            [4, 4, 1, 4, 4, 4, 6, 7, 7, 8, 8, 4]
        ],
        [ -- 8
            [6, 6, 5, 6, 6, 6, 6, 5, 5, 5, 5, 6],
            [8, 8, 8, 8, 8, 8, 8, 2, 2, 8, 8, 8],
            [4, 4, 3, 4, 4, 4, 4, 4, 4, 3, 3, 4],
            [4, 4, 1, 4, 4, 4, 6, 7, 7, 8, 8, 4]
        ],
        [ -- 9
            [4, 4, 3, 2, 2, 3, 8, 2, 2, 7, 7, 8],
            [8, 8, 8, 7, 7, 8, 6, 6, 6, 5, 5, 5],
            [6, 6, 5, 5, 5, 5, 3, 4, 4, 2, 2, 3],
            [4, 4, 1, 5, 5, 1, 6, 4, 4, 5, 5, 1]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = (c3v, c0r)

-- {{{2 c3v

c3v =     [
        [ -- 0
            [3, 3, 3, 5, 3, 2, 3, 3, 2, 7, 7, 8],
            [8, 8, 8, 2, 8, 7, 8, 8, 6, 5, 5, 5],
            [5, 5, 5, 5, 5, 5, 5, 5, 4, 2, 2, 3],
            [8, 8, 8, 7, 8, 5, 3, 3, 4, 5, 5, 1]
        ],
        [ -- 1
            [3, 3, 6, 5, 5, 5, 6, 6, 6, 5, 5, 5],
            [8, 8, 8, 7, 7, 8, 8, 8, 8, 7, 7, 8],
            [5, 5, 4, 2, 2, 3, 4, 4, 4, 2, 2, 3],
            [8, 8, 4, 5, 5, 1, 4, 4, 4, 5, 5, 1]
        ],
        [ -- 2
            [3, 3, 5, 4, 3, 4, 2, 3, 3, 2, 2, 3],
            [8, 8, 2, 8, 7, 8, 7, 8, 8, 7, 7, 8],
            [5, 5, 5, 6, 7, 6, 5, 5, 5, 5, 5, 5],
            [8, 8, 7, 6, 5, 4, 5, 3, 3, 5, 5, 1]
        ],
        [ -- 3
            [6, 5, 4, 4, 3, 2, 8, 3, 2, 2, 2, 3],
            [8, 7, 8, 2, 8, 7, 6, 8, 8, 7, 7, 8],
            [4, 2, 1, 5, 5, 5, 3, 5, 6, 5, 5, 5],
            [4, 5, 6, 7, 8, 5, 6, 3, 4, 5, 5, 1]
        ],
        [ -- 4
            [2, 2, 3, 2, 2, 3, 8, 8, 8, 7, 7, 8],
            [7, 7, 8, 7, 7, 8, 5, 6, 6, 5, 5, 5],
            [5, 5, 5, 5, 5, 5, 3, 2, 2, 2, 2, 3],
            [5, 5, 8, 5, 5, 1, 3, 4, 4, 5, 5, 1] -- CN 5, 5, *2, 5...
        ],
        [ -- 5
            [3, 3, 5, 4, 3, 4, 3, 3, 2, 2, 2, 3],
            [8, 8, 2, 8, 7, 8, 7, 8, 8, 7, 7, 8],
            [5, 5, 5, 6, 7, 6, 5, 5, 6, 5, 5, 5],
            [8, 8, 7, 6, 5, 4, 5, 3, 4, 5, 5, 1]
        ],
        [ -- 6
            [7, 7, 6, 7, 7, 8, 8, 8, 8, 7, 7, 8],
            [5, 5, 4, 5, 5, 5, 5, 6, 6, 5, 5, 5],
            [2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 3],
            [5, 5, 2, 5, 5, 1, 3, 4, 4, 5, 5, 1]
        ],
        [ -- 7
            [2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 3],
            [7, 7, 6, 7, 7, 8, 8, 8, 8, 7, 7, 8],
            [5, 5, 4, 5, 5, 5, 5, 6, 6, 5, 5, 5],
            [5, 5, 2, 5, 5, 1, 3, 4, 4, 5, 5, 1]
        ],
        [ -- 8
            [5, 5, 4, 5, 5, 2, 8, 8, 8, 7, 7, 8],
            [7, 7, 6, 7, 7, 7, 5, 6, 6, 5, 5, 5],
            [2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 3],
            [5, 5, 2, 5, 5, 5, 3, 4, 4, 5, 5, 1]
        ],
        [ -- 9
            [3, 4, 4, 3, 3, 4, 6, 6, 5, 4, 4, 3],
            [8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8],
            [5, 6, 6, 5, 5, 6, 4, 4, 3, 6, 6, 5],
            [8, 6, 4, 8, 8, 4, 4, 4, 1, 4, 4, 8]
        ]
    ]
-- }}}2
-- }}}1
