-- vim: set foldmethod=marker :

-- {{{1 metadata
{-|
Module      : Arca_musarithmica.Syntagma1.Pinax10
Description : Data for the ark, syntagma 1, pinax 10 (Phaleucium Hendecasyllabum)
Copyright   : Data from Kircher, 1650, implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark (bk II, p.
93): "Pro Metris Sapphicis".  This /pinax/ ('s1p10') is for poetry in
quatrains built of three 11-syllable lines followed by an "Adonic"
five-syllable line. There are four columns, one for each successive line (but
called "strophes"), where the fourth column is for the Adonic tag line.  

There is only one 'RpermTable' for the three 11-syllable columns, and a
separate one for the Adonic column.
-}
-- }}}1

module Arca_musarithmica.Syntagma1.Pinax10 (s1p10) where

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

s1p10 = fromList [c0, c1, c2, c3] :: Pinax

-- {{{1 c0
c0 = Column c0v c0r

-- {{{2 c0v
c0v :: VpermTable
c0v = buildVpermTable
    [
        [ -- 0
            [3, 3, 2, 8, 7, 6, 8, 8, 8, 7, 8],
            [8, 8, 7, 6, 5, 4, 5, 6, 4, 5, 5],
            [5, 5, 5, 3, 3, 8, 3, 4, 8, 2, 3],
            [8, 8, 5, 6, 3, 4, 8, 4, 6, 5, 1]
        ],
        [ -- 1
            [2, 3, 3, 2, 3, 3, 2, 4, 5, 4, 5],
            [7, 8, 8, 7, 8, 8, 8, 8, 7, 6, 7],
            [5, 5, 5, 5, 5, 5, 5, 8, 2, 2, 2],
            [5, 3, 1, 5, 1, 8, 7, 6, 5, 2, 5]
        ],
        [ -- 2
            [7, 8, 8, 7, 8, 8, 2, 3, 6, 5, 5],
            [5, 5, 5, 5, 5, 5, 5, 8, 8, 7, 8],
            [2, 2, 2, 2, 3, 3, 2, 2, 4, 2, 3],
            [1, 2, 5, 5, 1, 8, 7, 6, 4, 5, 1] -- #2 all unclear
        ],
        [ -- 3
            [3, 3, 2, 3, 2, 2, 2, 4, 6, 5, 5],
            [8, 8, 7, 8, 7, 8, 7, 8, 8, 7, 8],
            [5, 5, 5, 5, 5, 5, 5, 1, 4, 2, 3],
            [1, 1, 5, 1, 5, 8, 7, 6, 1, 2, 1] -- unclear
        ],
        [ -- 4
            [3, 2, 3, 4, 5, 5, 5, 3, 3, 2, 5], -- unclear
            [8, 7, 8, 8, 7, 7, 8, 7, 8, 7, 7],
            [5, 5, 5, 1, 2, 4, 6, 5, 4, 4, 5],
            [1, 5, 8, 6, 5, 3, 4, 5, 6, 7, 3]
        ],
        [ -- 5
            [8, 7, 8, 2, 3, 8, 7, 7, 2, 8, 7],
            [5, 5, 6, 7, 7, 6, 5, 5, 7, 5, 5],
            [3, 3, 3, 5, 5, 1, 2, 3, 4, 3, 2],
            [1, 2, 8, 7, 3, 4, 2, 3, 7, 1, 5]
        ],
        [ -- 6
            [2, 3, 3, 2, 3, 3, 2, 4, 6, 5, 5],
            [7, 8, 8, 7, 8, 8, 2, 8, 8, 7, 8],
            [5, 8, 5, 5, 5, 5, 5, 1, 4, 2, 3],
            [5, 3, 1, 5, 1, 8, 7, 6, 4, 5, 1]
        ],
        [ -- 7
            [3, 3, 4, 3, 2, 2, 2, 2, 8, 7, 8], -- unclear
            [8, 8, 8, 7, 7, 7, 7, 7, 5, 5, 5],
            [7, 5, 1, 2, 5, 4, 4, 4, 3, 2, 3],
            [8, 8, 6, 5, 5, 7, 7, 7, 8, 5, 1]
        ],
        [ -- 8 
            [8, 7, 8, 6, 8, 7, 3, 8, 3, 2, 3],
            [5, 5, 5, 4, 5, 5, 5, 6, 5, 5, 5],
            [3, 2, 3, 1, 3, 1, 8, 8, 8, 7, 8],
            [1, 5, 3, 4, 1, 5, 3, 4, 1, 5, 1]
        ],
        [ -- 9
            [3, 3, 2, 8, 7, 6, 8, 8, 8, 7, 8],
            [8, 8, 7, 6, 5, 4, 5, 6, 4, 5, 5],
            [5, 5, 5, 3, 3, 1, 3, 4, 1, 2, 3],
            [8, 8, 5, 6, 3, 4, 1, 4, 6, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r :: RpermTable
c0r = buildRpermTable
    [
        [ -- Duple
            [Sb, Mn, Mn, Mn, Mn, Sb, Mn, Mn, Mn, Sb, Sb],
            [Sb, Mn, Mn, Sb, Mn, Sb, Mn, Mn, Mn, Sb, Sb],
            [Mn, Sm, Sm, Mn, Sm, Fs, Fs, Sm, Sm, Mn, Sb],
            [Mn, Sm, Sm, Mn, Mn, SmD, Fs, Sm, Sm, Sb, Sb],
            [Sm, Fs, Fs, Sm, Sm, Mn, Mn, Sm, Sm, Mn, Sb],
            [Sb, Mn, Mn, Mn, Mn, Sb, Mn, Sb, Mn, Sb, Sb]
        ],
        [ -- TripleMajor
            [Sb, Sb, Sb, Br, Sb, Br, Sb, Br, Sb, BrD, BrD],
            [Sb, Sb, Sb, Br, Sb, Sb, Sb, Sb, Sb, Br, BrD],
            [Br, Sb, Sb, Sb, Sb, SbD, Mn, Sb, Sb, Br, BrD]
        ],
        [ -- TripleMinor
            [Mn, Mn, Mn, Sb, Mn, Sb, Mn, Sb, Mn, SbD, SbD],
            [Mn, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Mn, Sb, SbD] -- end: actually colored Mn, Sb, SbD
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
            [8, 8, 7, 8, 8, 2, 2, 8, 8, 6, 7],
            [5, 5, 5, 5, 6, 7, 7, 5, 5, 4, 5],
            [3, 3, 2, 3, 3, 4, 5, 3, 3, 2, 2], -- unclear
            [1, 1, 5, 1, 1, 7, 7, 1, 1, 2, 5]
        ],
        [ -- 1
            [5, 5, 5, 3, 5, 4, 3, 3, 5, 4, 5],
            [7, 7, 7, 8, 7, 7, 7, 8, 7, 7, 7],
            [3, 3, 3, 8, 3, 2, 5, 3, 3, 2, 3],
            [3, 2, 2, 6, 3, 7, 5, 6, 3, 7, 3] -- super unclear
        ],
        [ -- 2
            [5, 5, 5, 4, 5, 5, 4, 3, 2, 2, 2],
            [7, 7, 7, 7, 7, 7, 7, 8, 7, 6, 7],
            [3, 3, 3, 2, 3, 5, 4, 5, 5, 4, 5],
            [3, 3, 3, 2, 2, 3, 2, 5, 5, 2, 5] -- unclear
        ],
        [ -- 3
            [3, 3, 2, 4, 3, 4, 4, 3, 3, 2, 2],
            [8, 8, 7, 8, 8, 2, 2, 8, 8, 6, 7],
            [5, 5, 5, 6, 5, 2, 2, 5, 5, 4, 5],
            [8, 8, 5, 8, 8, 7, 7, 8, 8, 1, 1] -- super unclear
        ],
        [ -- 4
            [7, 7, 7, 8, 2, 8, 7, 6, 8, 6, 7],
            [5, 5, 4, 5, 7, 5, 5, 4, 5, 4, 5],
            [3, 3, 4, 3, 4, 3, 2, 2, 3, 2, 2],
            [3, 3, 2, 1, 7, 2, 5, 2, 1, 2, 5]
        ],
        [ -- 5
            [5, 5, 5, 4, 3, 2, 3, 8, 3, 2, 3],
            [7, 7, 7, 7, 5, 1, 7, 6, 7, 6, 7],
            [3, 3, 3, 2, 3, 4, 5, 3, 7, 4, 5],
            [3, 3, 3, 7, 8, 7, 3, 6, 5, 4, 3]
        ],
        [ -- 6
            [5, 5, 5, 4, 5, 7, 2, 3, 2, 2, 2],
            [7, 7, 7, 7, 7, 7, 7, 8, 7, 6, 7],
            [3, 3, 3, 2, 3, 3, 4, 5, 5, 4, 5],
            [3, 3, 3, 7, 3, 3, 2, 1, 5, 2, 5]
        ],
        [ -- 7
            [3, 4, 5, 4, 5, 3, 4, 5, 4, 4, 5], -- unclear
            [8, 8, 7, 8, 7, 8, 2, 7, 8, 7, 7],
            [5, 1, 3, 6, 5, 5, 2, 3, 3, 2, 3],
            [8, 6, 3, 4, 5, 8, 7, 5, 6, 7, 3]
        ],
        [ -- 8
            [3, 2, 2, 3, 5, 4, 3, 3, 5, 4, 5], -- unclear
            [8, 2, 7, 8, 7, 8, 7, 8, 7, 7, 7],
            [5, 5, 5, 1, 3, 6, 5, 1, 3, 2, 3],
            [1, 7, 7, 6, 3, 4, 5, 6, 3, 7, 3]
        ],
        [ -- 9
            [8, 8, 7, 8, 8, 2, 2, 8, 8, 6, 7],
            [5, 5, 5, 5, 6, 7, 7, 5, 5, 4, 5],
            [3, 3, 2, 3, 3, 4, 4, 3, 3, 2, 2],
            [1, 1, 2, 1, 3, 7, 7, 1, 1, 2, 5] -- unclear
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
            [2, 3, 3, 2, 3, 4, 2, 4, 6, 5, 5],
            [7, 8, 8, 7, 8, 8, 2, 8, 8, 7, 8],
            [5, 5, 5, 5, 5, 5, 5, 1, 4, 2, 3],
            [5, 3, 1, 5, 1, 8, 7, 6, 4, 5, 1]
        ],
        [ -- 1  
            [3, 3, 3, 4, 5, 1, 2, 8, 8, 7, 8],
            [8, 8, 8, 8, 7, 6, 7, 5, 5, 5, 5],
            [5, 5, 5, 2, 2, 4, 4, 3, 3, 2, 3], -- unclear
            [1, 1, 1, 6, 5, 4, 2, 3, 1, 5, 1]
        ],
        [ -- 2
            [3, 3, 3, 8, 7, 6, 8, 7, 8, 7, 8],
            [7, 7, 7, 6, 5, 4, 5, 5, 4, 5, 5],
            [5, 5, 3, 3, 3, 2, 3, 2, 1, 2, 3],
            [3, 3, 3, 6, 2, 4, 3, 5, 6, 5, 1] -- unclear
        ],
        [ -- 3  
            [3, 3, 2, 3, 3, 2, 5, 4, 3, 2, 3],
            [7, 7, 7, 7, 7, 7, 7, 6, 5, 5, 5],
            [5, 5, 4, 5, 5, 4, 3, 8, 8, 7, 8],
            [3, 3, 7, 5, 3, 7, 2, 4, 1, 5, 1]
        ],
        [ -- 4
            [8, 2, 8, 2, 2, 3, 5, 4, 3, 2, 3],
            [5, 5, 6, 7, 7, 8, 7, 6, 5, 5, 5],
            [3, 3, 3, 4, 4, 1, 3, 8, 8, 7, 8],
            [8, 8, 8, 7, 7, 6, 3, 4, 1, 5, 1]
        ],
        [ -- 5
            [3, 3, 3, 2, 3, 4, 5, 4, 4, 5, 5],
            [8, 8, 8, 7, 8, 2, 7, 6, 8, 7, 8],
            [5, 5, 5, 5, 5, 2, 3, 1, 1, 2, 3],
            [1, 1, 1, 5, 8, 7, 3, 4, 6, 5, 1]
        ],
        [ -- 6
            [3, 3, 3, 8, 7, 6, 8, 7, 8, 7, 8],
            [7, 7, 7, 6, 5, 4, 5, 5, 4, 5, 5],
            [5, 5, 5, 3, 3, 1, 3, 2, 1, 2, 3],
            [3, 3, 3, 6, 3, 4, 1, 5, 6, 5, 1]
        ],
        [ -- 7
            [3, 2, 8, 7, 3, 2, 8, 7, 8, 7, 8],
            [7, 7, 5, 5, 7, 6, 6, 5, 4, 5, 5],
            [5, 4, 3, 2, 3, 4, 4, 2, 1, 2, 3],
            [3, 7, 8, 5, 3, 4, 4, 5, 6, 4, 1]
        ],
        [ -- 8
            [2, 4, 3, 4, 5, 3, 2, 8, 8, 7, 8],
            [7, 8, 7, 8, 8, 8, 7, 5, 6, 5, 5],
            [4, 4, 5, 6, 5, 5, 4, 3, 2, 2, 3],
            [3, 6, 3, 4, 3, 1, 2, 3, 4, 5, 1]
        ],
        [ -- 9
            [2, 3, 3, 2, 3, 3, 2, 4, 6, 5, 5],
            [7, 8, 8, 7, 8, 8, 2, 8, 8, 7, 8],
            [5, 5, 5, 5, 5, 5, 5, 1, 4, 2, 3],
            [5, 4, 3, 5, 1, 6, 7, 6, 4, 5, 1]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = Column c3v c3r

-- {{{2 c3v
c3v :: VpermTable
c3v = buildVpermTable
    [
        [ -- 0
            [3, 2, 4, 5, 5],
            [8, 2, 6, 7, 6],
            [5, 5, 1, 2, 3],
            [8, 7, 6, 5, 1]
        ],
        [ -- 1
            [8, 8, 8, 7, 8],
            [5, 5, 6, 5, 5],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 2
            [8, 8, 3, 2, 3],
            [6, 6, 8, 7, 8],
            [4, 4, 5, 5, 5],
            [4, 4, 1, 5, 1]
        ],
        [ -- 3
            [3, 3, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [5, 5, 5, 5, 5],
            [8, 8, 8, 5, 1]
        ],
        [ -- 4
            [7, 8, 8, 7, 8],
            [5, 5, 5, 5, 5],
            [2, 3, 3, 2, 3],
            [5, 2, 1, 5, 1]
        ],
        [ -- 5
            [5, 3, 6, 5, 5],
            [7, 8, 8, 7, 8],
            [2, 1, 4, 2, 3],
            [7, 6, 4, 5, 1]
        ],
        [ -- 6
            [8, 2, 3, 2, 3],
            [5, 7, 8, 7, 8],
            [3, 4, 5, 5, 5],
            [5, 2, 1, 5, 1]
        ],
        [ -- 7
            [6, 8, 8, 7, 8],
            [4, 5, 5, 5, 5],
            [8, 2, 2, 3, 2],
            [4, 3, 3, 5, 1]
        ],
        [ -- 8 
            [3, 2, 4, 5, 5],
            [8, 2, 8, 7, 8],
            [5, 5, 1, 2, 3],
            [8, 7, 6, 5, 1]
        ],
        [ -- 9
            [6, 5, 5, 6, 5],
            [8, 8, 8, 8, 8],
            [4, 3, 3, 4, 3],
            [4, 8, 8, 4, 1]
        ]
    ]
-- }}}2
-- {{{2 c3r
c3r :: RpermTable
c3r = buildRpermTable
    [
        [ -- Duple
            [MnD, Fs, Fs, Sb, Sb],
            [SbD, Sm, Sm, Sb, Sb],
            [Mn, Sm, Sm, Sb, Sb],
            [MnR, Sb, Mn, Sb, Sb, Sb],
            [SmR, Mn, Fs, Fs, Sb, Sb], -- unclear
            [MnD, Sm, Sb, Sb, Sb],
            [Sm, Fs, Fs, Sm, Sm]
        ],
        [ -- TripleMajor
            [SbD, Mn, Sb, Br, Sb],
            [Sb, Sb, Sb, Br, Sb]
        ],
        [ -- TripleMinor
            [Mn, Mn, Mn, Sb, Mn]
        ]
    ]
-- }}}1