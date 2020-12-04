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
-- {{{2 c0r
c0r :: RpermTable
c0r = buildRpermTable
    [
        [ -- Duple
            [Sb, SbD, Mn, Mn, Mn, Sb, Sb],
            [Mn, Sb, Sb, Sb, Mn, Sb, Sb],
            [Mn, Sm, Sm, Sm, Sm, Mn, Sb],
            [Mn, SmD, Fs, SmD, Fs, Mn, Sb],
            [Mn, Sb, Mn, Mn, Mn, Sb, Sb],
            [Sb, MnD, Sm, Sm, Sm, Mn, Sb],
            [SmR, Sm, Sm, Sm, Mn, Sm, Sb, Sb], 
            [Sm, Sm, Sm, MnD, Sm, Sb, Sb], -- first few cols have prev perm, rest have this
            [Sm, Mn, Sm, Sm, Sm, Mn, Sb],
            [Sm, Fs, Fs, Sm, Sm, Mn, Mn]
        ],
        [ -- TripleMajor
            [Sb, Sb, Sb, Br, Sb, BrD, BrD], -- penultimate dot implied
            [BrD, Br, Sb, Br, Sb, BrD, BrD], -- first dot implied
            [Sb, Sb, Sb, Sb, Sb, Sb, BrD]
        ],
        [ -- TripleMinor
            [Mn, Mn, Mn, Sb, Mn, SbD, SbD], -- penultimate dot implied
            [Mn, Sb, Mn, Sb, Mn, Sb, SbD]
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
