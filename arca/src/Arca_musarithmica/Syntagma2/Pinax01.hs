{-|
Module      : Arca_musarithmica.Syntagma2.Pinax01
Description : Data for the ark, syntagma 2, pinax 1
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p1') is for hexasyllabic Adonic and Dactylic meters:

"Pinax 1. Melotheticus. In quo numeri harmonici pro metris Adoniis & Dactylicis inaequali quidem, sed Florida & Artificioso vocum progressu disponuntur."

It contains four columns for successive strophes. Each contains pairs of four-voice vperms and four-voice rperms.

It is to be used with /tonoi/ 5, 6, 7, 8, 11, and 12. (__TODO__)
-}

module Arca_musarithmica.Syntagma2.Pinax01 (s2p1) where

import Aedifico
    (
        Dur (..),
        Column (..),
        buildVpermTable,
        buildRpermTable,
        buildPinax
    )

s2p1 = buildPinax [c0]

-- {{{1 c0
c0 = Column c0v c0r

-- {{{2 c0v
c0v = buildVpermTable 
    [ 
        [ -- 0
            [5, 4, 3, 2, 3],
            [3, 2, 1, 7, 1, 7, 1],
            [5, 5, 5, 5, 5],
            [8, 7, 6, 5, 1]
        ],
        [ -- 1
            [5, 4, 3, 2, 3],
            [3, 3, 8, 2, 7, 8, 7, 8],
            [5, 5, 6, 2, 3, 4, 5, 5],
            [8, 8, 4, 5, 1]
        ],
        [ --2
            [8, 8, 6, 2, 8, 7, 8, 7, 8],
            [6, 6, 6, 5, 5],
            [4, 4, 4, 4, 3, 2, 3],
            [4, 4, 4, 5, 1]
        ],
        [ --3
            [3, 4, 5, 3, 4, 3, 2, 1, 1, 7, 1],
            [8, 2, 3, 8, 8, 8, 6, 5, 5],
            [5, 6, 5, 4, 3, 2, 3],
            [8, 4, 5, 6, 7, 8, 4, 5, 1]
        ],
        [ --4
            [3, 2, 2, 8, 7, 8, 2, 4, 3, 3, 2, 3],
            [8, 6, 6, 6, 6, 2, 8, 8, 7, 8],
            [5, 4, 4, 3, 4, 6, 5, 5, 5, 5],
            [1, 2, 2, 6, 2, 3, 3, 4, 5, 1]
        ],
        [ -- 5
            [8, 4, 3, 2, 1, 1, 7, 1],
            [6, 6, 6, 5, 5],
            [8, 2, 8, 4, 3, 3, 2, 1, 2, 3],
            [4, 4, 4, 5, 1]
        ],
        [ --6
            [6, 6, 5, 6, 5, 4, 5],
            [8, 8, 8, 8, 8],
            [4, 4, 3, 4, 8, 4, 3, 2, 3],
            [4, 4, 1, 4, 1]
        ],
        [ -- 7
            [8, 8, 8, 4, 3, 2, 3],
            [6, 6, 6, 7, 1, 7, 6, 7, 1],
            [4, 4, 4, 2, 3, 4, 5, 5],
            [4, 4, 4, 5, 1]
        ],
        [ -- 8
            [4, 4, 4, 4, 3, 2, 3],
            [8, 8, 8, 2, 7, 8, 7, 8],
            [6, 6, 6, 5, 5],
            [4, 4, 4, 5, 1]
        ],
        [ --9
            [5, 5, 4, 5, 5],
            [7, 7, 8, 7, 6, 7, 8],
            [2, 2, 8, 2, 3],
            [5, 5, 6, 5, 1]
        ]
    ]


-- }}}2
-- {{{2 c0r
c0r = buildRpermTable 
    [ 
        [ -- duple 
            [ -- 0
                [SmR, SbD, Sm, Sm, Sb, Sb],
                [SbD, Sm, Sm, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Br, Sm],
                [SbD, Sm, Sm, Br, Sm]
            ],
            [ -- 1
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 2
                [MnD, Sm, Sm, Sm, Sm, Sm, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Mn, Sb, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 3
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 4
                [MnD, Sm, BrD, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 5
                [MnD, Fs, Fs, Sb, Sb],
                [MnD, Fs, Fs, Sb, Sb],
                [MnD, Fs, Fs, Sb, Sb],
                [MnD, Fs, Fs, Sb, Sb] -- not florid, right?
            ],
            [ -- 6
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Mn, Sm, Sm, Br, Br],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 7
                [MnR, SbD, Sm, Sm, Sm, Sm, Mn, Sb],
                [SbD, Sm, Sm, Mn, Mn, Sm, Mn, Sm, Sb],
                [SbR, Sb, Sm, Sm, Sm, Mn, Sb], -- XXX or is it two fusae in the middle?
                [SbD, Sm, Sm, Br, Sb]
            ],
            [ -- 8
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Mn, Sm, Sm, Sb, Sb],
                [MnD, Sm, Mn, Sb, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 9
                [SmR, Mn, Sm, Mn, Mn, Sm, Mn, Sm, Sb],
                [SmR, Mn, Sm, Sb, Sb, Sb],
                [SmR, Mn, Sm, Mn, MnD, Sm, Mn, Sb],
                [SmR, Mn, Sm, Br, Br, Br]
            ]
        ]
    ]
-- }}}2
-- }}}1

