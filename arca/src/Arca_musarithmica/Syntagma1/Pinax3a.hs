-- vim: set foldmethod=marker :

-- {{{1 metadata
{-|
Module      : Arca_musarithmica.Syntagma1.Pinax3a
Description : Data for the ark, syntagma 1, pinax 3a (Adonius)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s1p3a') is for poetry in Adonic meter ('__'_).
It contains four columns.
The rperms are identical in all four columns.
-}
-- }}}1

module Arca_musarithmica.Syntagma1.Pinax3a (s1p3a) where

-- {{{1 imports
import Data.Vector 
    (fromList)

import Aedifico
    (Dur (..),
     Pinax,
     Column,
     VpermTable,
     RpermTable,
     buildVpermTable,
     buildRpermTable)
-- }}}1

s1p3a = fromList [c0, c1, c2, c3] :: Pinax

-- {{{1 c0
c0 = (c0v, c0r) :: Column

-- {{{2 c0v
c0v :: VpermTable
c0v = buildVpermTable
    [
        [ -- 0
            [5, 5, 6, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 1
            [3, 2, 4, 5, 5],
            [8, 2, 8, 2, 3],
            [5, 5, 6, 7, 8],
            [8, 7, 6, 5, 1]
        ],
        [ -- 2
            [5, 5, 5, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 3, 2, 3],
            [8, 8, 8, 5, 1]
        ],
        [ -- 3
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 8, 5, 1] -- XXX unclear
        ],
        [ -- 4
            [6, 5, 5, 6, 5],
            [8, 8, 8, 8, 8],
            [8, 5, 5, 4, 3],
            [4, 3, 3, 4, 1]
        ],
        [ -- 5
            [3, 3, 2, 3, 3],
            [8, 8, 7, 8, 8],
            [5, 5, 5, 5, 5],
            [8, 8, 5, 1, 1]
        ],
        [ -- 6
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 1, 5, 1]
        ],
        [  -- 7
            [5, 5, 6, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 8
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 8, 5, 1]
        ],
        [ -- 9
            [5, 5, 5, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 3, 2, 3],
            [8, 8, 8, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r :: RpermTable
c0r = buildRpermTable
    [
        [ -- Duple
            [MnD, Fs, Fs, Sb, Sb],
            [SbD, Sm, Sm, Sb, Sb],
            [Mn, Sm, Sm, Mn, Mn],
            [Sb, Mn, Mn, Sb, Sb],
            [Mn, Sb, Mn, Sb, Sb],
            [MnD, Sm, Mn, Sb, Mn],
            [MnR, Sb, Sm, Sm, Sb, Sb],
            [FsR, Mn, Fs, Fs, Mn, Mn],
            [Sm, Fs, Fs, Sm, Sm]
        ],
        [ -- TripleMajor
            [SbD, Mn, Sb, Br, Sb],
            [Sb, Sb, Sb, Sb, Sb, SbR],
            [Br, Sb, Sb, Br, BrD],
            [Sb, Mn, Mn, Sb, BrD]
        ],
        [ -- TripleMinor
            [MnD, Sm, Mn, Sb, Mn],
            [Mn, Mn, Mn, Mn, Mn, MnR],
            [Mn, Mn, Mn, Sb, Mn]
        ]
    ]
-- }}}2
-- }}}1

-- {{{1 c1
c1 = (c1v, c1r) :: Column

-- {{{2 c1v
c1v :: VpermTable
c1v = buildVpermTable [
        [ -- 0
            [6, 6, 6, 6, 6],
            [8, 8, 2, 8, 2],
            [4, 4, 4, 3, 4],
            [4, 4, 2, 6, 2]
        ],
        [ -- 1
            [4, 4, 3, 2, 1],
            [8, 8, 8, 6, 6],
            [6, 6, 5, 4, 3],
            [4, 4, 1, 2, 6]
        ],
        [ -- 2
            [2, 2, 2, 8, 2],
            [6, 6, 7, 6, 6],
            [4, 4, 5, 3, 4],
            [2, 2, 5, 6, 2]
        ],
        [ -- 3
            [6, 6, 5, 4, 3],
            [8, 8, 3, 2, 8],
            [6, 6, 8, 6, 6],
            [4, 4, 8, 2, 6]
        ],
        [ -- 4
            [2, 2, 2, 2, 2],
            [7, 7, 6, 7, 7],
            [5, 5, 4, 5, 5],
            [5, 5, 2, 5, 5]
        ],
        [ -- 5
            [4, 3, 2, 7, 8],
            [2, 8, 6, 5, 5],
            [6, 5, 4, 2, 3],
            [2, 3, 4, 5, 1]
        ],
        [ -- 6
            [2, 2, 3, 2, 2],
            [7, 7, 8, 6, 7],
            [5, 5, 5, 4, 5],
            [5, 5, 1, 2, 5]
        ],
        [ -- 7 
            [6, 5, 5, 6, 5],
            [8, 8, 8, 8, 8],
            [8, 5, 5, 4, 3],
            [4, 3, 3, 4, 1]
        ],
        [ -- 8
            [6, 6, 6, 6, 6],
            [8, 8, 2, 8, 2],
            [4, 4, 4, 3, 4],
            [4, 4, 2, 6, 2]
        ],
        [ -- 9
            [4, 4, 3, 2, 1],
            [8, 8, 8, 6, 6],
            [6, 6, 5, 4, 3],
            [4, 4, 1, 2, 6]
        ]
    ]
-- }}}2

c1r = c0r :: RpermTable
-- }}}1

-- {{{1 c2
c2 = (c2v, c2r) :: Column

-- {{{2 c2v
c2v :: VpermTable
c2v = buildVpermTable [
        [ -- 0
            [5, 5, 5, 4, 5],
            [2, 2, 3, 2, 2],
            [7, 7, 8, 6, 7],
            [5, 5, 1, 2, 5]
        ],
        [ -- 1
            [2, 2, 2, 8, 2],
            [6, 6, 7, 6, 6],
            [4, 4, 5, 3, 3],
            [2, 2, 5, 6, 2]
        ],
        [ -- 2
            [6, 6, 5, 4, 3],
            [8, 8, 3, 2, 8],
            [6, 6, 8, 6, 6],
            [4, 4, 8, 2, 6]
        ],
        [ -- 3
            [5, 5, 6, 5, 5],
            [8, 8, 8, 8, 8],
            [3, 3, 4, 3, 3],
            [8, 8, 4, 8, 8]
        ],
        [ -- 4
            [5, 5, 5, 5, 5],
            [3, 3, 2, 3, 3],
            [8, 8, 7, 8, 8],
            [8, 8, 5, 5, 1]
        ],
        [ -- 5
            [5, 5, 6, 5, 5],
            [8, 8, 7, 8, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 6
            [5, 5, 5, 5, 5],
            [8, 8, 8, 8, 8],
            [3, 3, 3, 2, 3],
            [8, 8, 8, 5, 1]
        ],
        [ -- 7 
            [6, 5, 5, 6, 5],
            [8, 8, 8, 8, 8],
            [8, 5, 5, 4, 3],
            [4, 3, 3, 4, 1]
        ],
        [ -- 8
            [5, 5, 5, 5, 5],
            [8, 8, 7, 8, 8],
            [3, 3, 2, 3, 3],
            [8, 8, 5, 1, 1]
        ],
        [ -- 9
            [5, 5, 4, 5, 5],
            [3, 2, 1, 2, 3],
            [5, 5, 6, 7, 8],
            [8, 7, 6, 5, 1]
        ]
    ]
-- }}}2

c2r = c0r :: RpermTable
-- }}}1

-- {{{1 c3
c3 = (c3v, c3r) :: Column

-- {{{2 c3v
c3v :: VpermTable
c3v = buildVpermTable [
        [ -- 0
            [5, 5, 4, 5, 5],
            [3, 2, 1, 2, 3],
            [5, 5, 6, 7, 8],
            [8, 7, 6, 5, 1]
        ],
        [ -- 1
            [5, 5, 6, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 2
            [2, 2, 3, 2, 2],
            [7, 7, 8, 6, 7],
            [5, 5, 5, 4, 5],
            [5, 5, 8, 2, 5]
        ],
        [ -- 3
            [4, 3, 8, 7, 8],
            [2, 8, 6, 5, 5],
            [6, 5, 4, 2, 3],
            [2, 3, 4, 5, 1]
        ],
        [ -- 4
            [5, 5, 6, 5, 5],
            [8, 8, 8, 8, 8],
            [3, 3, 4, 3, 3],
            [8, 8, 4, 1, 1]
        ],
        [ -- 5
            [2, 2, 2, 2, 2],
            [7, 7, 6, 7, 7],
            [5, 5, 4, 5, 5],
            [5, 5, 2, 5, 5]
        ],
        [ -- 6
            [3, 3, 2, 3, 3],
            [8, 8, 7, 8, 8],
            [5, 5, 5, 5, 5],
            [8, 8, 5, 1, 1]
        ],
        [ -- 7 
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 2, 5, 1]
        ],
        [ -- 8
            [5, 5, 6, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 9
            [4, 3, 2, 7, 8],
            [2, 8, 6, 5, 5],
            [6, 5, 4, 2, 3],
            [2, 3, 4, 5, 1]
        ]
    ]
-- }}}2

c3r = c0r :: RpermTable
-- }}}1
