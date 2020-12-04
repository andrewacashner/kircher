-- vim: set foldmethod=marker :

-- {{{1 metadata
{-|
Module      : Arca_musarithmica.Syntagma1.Pinax6
Description : Data for the ark, syntagma 1, pinax 6 (Iambicum Archilochicum)
Copyright   : Data from Kircher, 1650, implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark (bk II, p.
87): "Iambica Archilochica octosyllaba penultima breuia".  This /pinax/
('s1p6') is for poetry in Iambic meter, 8-syllable lines with short
penultimate. There are four columns, one for each successive line (but called
"strophes").  There is only one 'RpermTable' for all four columns.
-}
-- }}}1

module Arca_musarithmica.Syntagma1.Pinax6 (s1p6) where

-- {{{1 imports
import Data.Vector
    (fromList)

import Aedifico
    (Dur    (..),
     Pinax,
     Column (..),
     VpermTable,
     RpermTable,
     buildVpermTable,
     buildRpermTable)
-- }}}1

s1p6 = fromList [c0, c1, c2, c3] :: Pinax

-- {{{1 c0
c0 = Column c0v c0r

-- {{{2 c0v
c0v :: VpermTable
c0v = buildVpermTable
    [
        [ -- 0
            [5, 5, 5, 4, 3, 2, 2, 1],
            [8, 7, 8, 8, 8, 7, 7, 8],
            [3, 2, 3, 6, 5, 5, 5, 5],
            [8, 5, 3, 4, 1, 5, 5, 1]
        ],
        [ -- 1
            [3, 2, 4, 3, 3, 2, 2, 3],
            [8, 7, 2, 8, 8, 7, 7, 8],
            [5, 5, 6, 5, 5, 5, 5, 5],
            [1, 5, 2, 3, 1, 5, 5, 1]
        ],
        [ -- 2
            [3, 3, 2, 8, 8, 7, 7, 8],
            [8, 8, 6, 5, 6, 5, 5, 5],
            [5, 5, 4, 3, 2, 2, 2, 3],
            [1, 1, 2, 3, 4, 5, 5, 1]
        ],
        [ -- 3
            [3, 8, 8, 2, 3, 2, 2, 3],
            [5, 6, 5, 6, 8, 7, 7, 8],
            [1, 1, 1, 4, 5, 5, 5, 5],
            [1, 4, 3, 2, 1, 5, 5, 1]
        ],
        [ -- 4
            [8, 8, 8, 8, 3, 2, 2, 3],
            [5, 6, 5, 6, 5, 5, 5, 5],
            [3, 3, 3, 8, 8, 7, 7, 8],
            [8, 6, 3, 4, 1, 5, 5, 1]
        ],
        [ -- 5
            [3, 2, 5, 4, 3, 2, 2, 3],
            [8, 7, 8, 8, 8, 7, 7, 8],
            [5, 5, 5, 6, 5, 5, 5, 5],
            [1, 5, 3, 4, 1, 5, 5, 1]
        ],
        [ -- 6
            [3, 8, 8, 2, 3, 2, 2, 3],
            [5, 6, 5, 6, 8, 7, 7, 8],
            [8, 8, 2, 4, 5, 5, 5, 5],
            [1, 4, 3, 2, 1, 5, 5, 1]
        ],
        [ -- 7
            [3, 4, 3, 8, 4, 3, 3, 4],
            [8, 8, 7, 6, 8, 8, 8, 8],
            [5, 6, 5, 3, 6, 5, 5, 6], -- unclear XXX
            [8, 4, 5, 6, 5, 1, 1, 4]
        ],
        [ -- 8 
            [3, 2, 4, 3, 3, 2, 2, 3],
            [8, 7, 2, 8, 8, 7, 7, 8],
            [5, 5, 6, 5, 5, 5, 5, 5],
            [1, 5, 2, 3, 1, 5, 5, 1]
        ],
        [ -- 9
            [3, 4, 5, 4, 3, 2, 2, 3],
            [8, 8, 8, 8, 8, 7, 7, 8],
            [5, 6, 5, 6, 5, 5, 5, 5],
            [8, 6, 3, 4, 1, 5, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r :: RpermTable
c0r = buildRpermTable
    [
        [ -- Duple
            [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
            [Mn, Sm, Sm, Sm, Sm, SmD, Fs, Sb],
            [Mn, Sb, Mn, SbD, Mn, SbD, Mn, Sb],
            [Sm, Fs, Fs, Sm, Sm, SmD, Fs, Mn],
            [Sm, Mn, Sm, Sm, Sm, SmD, Fs, Sb],
            [MnR, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Sb],
            [SmR, Sm, Sm, Sm, MnD, Sm, MnD, Sm, Sb],
            [Mn, Sb, Mn, Sm, Sm, SmD, Fs, Sb]
        ],
        [ -- TripleMajor
            [Sb, Sb, Sb, Br, Sb, Br, Sb, BrD],
            [Br, Sb, Sb, Sb, Sb, Br, Sb, BrD],
            [Br, Sb, Sb, Sb, Sb, SbD, SbD, SbD],
        ],
        [ -- TripleMinor
            [Mn, Mn, Mn, Sb, Mn, Sb, Mn, SbD],
            [Sb, Mn, Mn, Mn, Mn, Sb, Mn, SbD]
        ]
    ]
-- }}}1
-- {{{1 c1
c1 = Column c1v c0r

-- {{{2 c1v
c1v :: VpermTable
c1v = buildVpermTable
    [
        [ -- 0
            [3, 4, 3, 1, 4, 3, 3, 4],
            [8, 8, 7, 6, 8, 8, 8, 8],
            [5, 6, 5, 3, 6, 5, 5, 6],
            [8, 4, 5, 6, 4, 1, 1, 4]
        ],
        [ -- 1
            [3, 3, 2, 3, 5, 5, 4, 5],
            [8, 8, 7, 8, 2, 8, 8, 2],
            [5, 5, 5, 5, 5, 6, 6, 7],
            [8, 8, 5, 8, 7, 6, 6, 5]
        ],
        [ -- 2
            [5, 5, 5, 8, 7, 6, 6, 7],
            [8, 8, 5, 6, 2, 2, 2, 2],
            [3, 3, 2, 3, 5, 4, 4, 5],
            [8, 8, 7, 6, 5, 2, 2, 5]
        ],
        [ -- 3
            [5, 5, 4, 3, 2, 2, 2, 2],
            [2, 3, 2, 8, 7, 6, 6, 7],
            [7, 8, 6, 5, 5, 4, 4, 5],
            [5, 1, 2, 3, 5, 2, 2, 5]
        ],
        [ -- 4
            [3, 3, 2, 2, 8, 2, 2, 2],
            [8, 8, 7, 6, 5, 6, 6, 7],
            [5, 5, 5, 4, 5, 4, 4, 5],
            [1, 1, 5, 2, 3, 2, 2, 5]
        ],
        [ -- 5
            [5, 6, 5, 4, 6, 5, 5, 6],
            [8, 8, 7, 8, 8, 8, 8, 8],
            [3, 4, 2, 8, 4, 3, 3, 4],
            [8, 4, 5, 6, 4, 1, 1, 4]
        ],
        [ -- 6
            [8, 7, 8, 7, 3, 2, 2, 3],
            [5, 5, 4, 5, 5, 5, 5, 5],
            [5, 2, 1, 2, 1, 7, 7, 8],
            [8, 5, 6, 5, 1, 5, 5, 1]
        ],
        [ -- 7
            [3, 2, 4, 3, 3, 2, 2, 3],
            [8, 7, 2, 8, 8, 7, 7, 8],
            [5, 5, 6, 5, 5, 5, 5, 5],
            [1, 5, 2, 3, 1, 5, 5, 1]
        ],
        [ -- 8
            [3, 3, 2, 3, 5, 5, 4, 5],
            [8, 8, 7, 8, 2, 8, 8, 2],
            [5, 5, 5, 5, 5, 6, 6, 7],
            [8, 8, 5, 8, 7, 6, 6, 5]
        ],
        [ -- 9
            [3, 2, 4, 3, 3, 2, 2, 3], -- unclear
            [8, 7, 2, 8, 8, 7, 7, 8],
            [5, 5, 6, 5, 5, 5, 5, 5],
            [1, 5, 2, 3, 1, 5, 5, 1]
        ]

    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = Column c2v c0r

-- {{{2 c2v
c2v :: VpermTable
c2v = buildVpermTable
    [
        [ -- 0
        ],
        [ -- 1
        ],
        [ -- 2
        ],
        [ -- 3
        ],
        [ -- 4
        ],
        [ -- 5
        ],
        [ -- 6
        ],
        [ -- 7
        ],
        [ -- 8 
        ],
        [ -- 9
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = Column c3v c0r

-- {{{2 c3v
c3v :: VpermTable
c3v = buildVpermTable
    [
        [ -- 0
        ],
        [ -- 1
        ],
        [ -- 2
        ],
        [ -- 3
        ],
        [ -- 4
        ],
        [ -- 5
        ],
        [ -- 6
        ],
        [ -- 7
        ],
        [ -- 8
        ],
        [ -- 9
        ]
    ]
-- }}}2
-- }}}1
