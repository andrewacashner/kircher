{-|
Module      : Arca_musarithmica.Syntagma2.Pinax01
Description : Data for the ark, syntagma 2, pinax 1
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p1') contains four columns for successive strophes. Each
contains pairs of four-voice vperms and four-voice rperms.

In one place (p. 103) Kircher says it is for inequal Adonic and Dactylic meters: 

> "Pinax 1. Melotheticus. In quo numeri harmonici pro metris Adoniis &
> Dactylicis inaequali quidem, sed Florida & Artificioso vocum progressu
> disponuntur."

In the introduction to Syntagma II (p. 102), though, he says it is for
hectasyllabic Adonic and Dactylic meters:

> "/Primus Pinax/ continet Musarithmos floridos & artificiosos pro metris Adonijs & Dactylicis hectasyllabis."

Either way, all the permutations are for /five/ syllables except for the florid (melismatic) voices.

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

s2p1 = buildPinax [c0, c1, c2, c3]

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
                [SbR, SbD, Sm, Sm, Sb, Sb],
                [SbD, Sm, Sm, Mn, Sb, Mn, Sb],
                [Br, Mn, Mn, Sb, Sb],
                [SbD, Sm, Sm, Br, Sm]
            ],
            [ -- 1
                [MnR, Sb, Sb, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Mn, Sm, Sm, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 2
                [MnD, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Sm, Sb],
                [MnD, Sm, Sb, Sb, Sb],
                [MnD, Sm, Mn, MnD, Sm, Mn, Sb],
                [MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 3
                [Sm, Sm, Sm, Sm, Sb, Mn, Sm, Sm, Mn, Mn, Sb],
                [Sm, Sm, Sm, Sm, Sb, Mn, Mn, Sb, Sb],
                [Sb, Sb, Mn, Sm, Sm, Sb, Sb],
                [Sb, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 4
                [Sb, Mn, MnD, Fs, Fs, Sm, Sm, Sm, Sm, Sm, Sb, Sb],
                [Sb, Mn, Mn, Sb, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Sb, Sb, Mn, Sm, Sm, Sb, Sb]
            ],
            [ -- 5
                [Mn, MnD, Sm, Sm, Sm, Mn, Mn, Sb],
                [MnD, Sm, Sb, Sb, Sb],
                [Mn, MnD, Sm, Sm, Sm, Sm, Fs, Fs, Mn, Sb],
                [MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 6
                [SmR, Mn, Sm, Sb, SbD, Sm, Sm, Sb],
                [SmR, Mn, Sm, Sb, Br, Sb],
                [SmR, Mn, Sm, Sb, Sm, Sm, Sb, Sm, Sm, Sb],
                [SmR, Mn, Sm, Sb, Sb, Br]
            ],
            [ -- 7
                [SmR, Mn, Sm, Mn, Sb, Mn, Sb, Sb],
                [SmR, Mn, Sm, Sb, Mn, MnD, Fs, Fs, Mn, Sb],
                [SmR, Mn, Sm, Mn, Sb, Sm, Sm, Sb, Sb],
                [SmR, Mn, Sm, Sb, Br, Sb]
            ],
            [ -- 8
                [SmR, Mn, Sm, Mn, MnD, Sm, Mn, Sb],
                [SmR, Mn, Sm, Mn, Mn, Sm, Mn, Sm, Sb],
                [SmR, Mn, Sm, Sb, Sb, Sb],
                [SmR, Mn, Sm, Sb, Sb, Sb]
            ],
            [ -- 9
                [Sm, Sm, Mn, Sb, Sb],
                [Sm, Sm, SbD, Fs, Fs, Sm, Sb],
                [Sm, Sm, Mn, Sb, Sb],
                [Sm, Sm, Mn, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c1
c1 = Column c1v c1r

-- {{{2 c1v
c1v = buildVpermTable 
    [ 
        [ -- 0
            [7, 8, 2, 8, 7, 6, 7],
            [5, 6, 7, 6, 5, 4, 5, 4, 5],
            [2, 2, 2, 2, 2],
            [5, 4, 3, 2, 5]
        ],
        [ -- 1
            [2, 2, 3, 2, 2],
            [7, 7, 8, 8, 7, 6, 7],
            [5, 5, 5, 4, 5, 4, 5],
            [5, 5, 1, 2, 5]
        ],
        [ --2
            [3, 3, 3, 4, 5, 4, 5],
            [1, 1, 1, 1, 7, 1, 2, 2],
            [5, 5, 5, 6, 5, 6, 7],
            [8, 8, 8, 2, 5]
        ],
        [ --3
            [8, 2, 3, 4, 5, 3, 2, 3],
            [5, 5, 5, 8, 7, 8, 7, 8],
            [3, 2, 8, 2, 3, 4, 5, 5],
            [8, 7, 6, 5, 1]
        ],
        [ --4
            [5, 5, 6, 5, 5],
            [8, 8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 5
            [4, 4, 3, 2, 3],
            [8, 8, 8, 8, 7, 6, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 1, 5, 1]
        ],
        [ --6
            [8, 8, 8, 2, 7, 8, 7, 6, 7, 8],
            [6, 6, 6, 5, 5],
            [4, 4, 4, 4, 3, 3, 2, 8, 2, 3],
            [4, 4, 4, 5, 1]
        ],
        [ -- 7
            [4, 4, 4, 3, 2, 1, 1, 7, 1],
            [8, 8, 8, 6, 5, 5],
            [6, 6, 6, 5, 4, 3, 2, 3],
            [4, 4, 4, 5, 1]
        ],
        [ -- 8
            [3, 2, 4, 3, 2, 4, 3, 2, 3],
            [8, 6, 6, 6, 4, 6, 5, 5, 5],
            [5, 4, 2, 8, 7, 8, 2, 8, 7, 8],
            [1, 2, 2, 6, 2, 3, 4, 5, 1]
        ],
        [ --9
            [2, 2, 3, 2, 2],
            [7, 7, 1, 1, 7, 6, 7],
            [5, 5, 5, 4, 5, 4, 5],
            [5, 5, 1, 2, 5]
        ]
    ]

-- }}}2
-- {{{2 c1r
c1r = buildRpermTable 
    [ 
        [ -- duple 
            [ -- 0
                [MnD, Sm, SbD, Sm, Sm, Sb, Sb],
                [MnD, Sm, Mn, Sm, Sm, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Br, Sb],
                [SbD, Sm, Sm, Br, Sb]
            ],
            [ -- 1
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Br, Mn, Mn, Sb, Sb], -- third note from end could be Sm
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 2
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Mn, Sm, Sm, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 3
                [MnD, Sm, Mn, Mn, Sm, Sm, Mn, Sb],
                [MnD, Sm, Sb, Sm, Mn, Sm, Sb],
                [MnD, Sm, Sb, Sm, Fs, Fs, Mn, Sb], 
                [MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 4
                [MnD, Sm, Sb, Sb, Sb],
                [MnD, Sm, Sb, Mn, Mn, Sb],
                [MnD, Sm, Sb, Sb, Sb],
                [MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 5
                [Mn, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, Sm, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, Sb, Sb]
            ],
            [ -- 6
                [MnD, Sm, Mn, Mn, Mn, MnD, Fs, Fs, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Mn, MnD, Sm, Sm, Fs, Fs, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 7
                [SmR, Mn, Sm, Sm, Sm, Sm, Sm, Mn, Mn, Sb],
                [SmR, Mn, Sm, Mn, Mn, Sb, Sb],
                [SmR, Mn, Sm, Sm, Sm, Sm, Sm, Sb, Sb],
                [SmR, Mn, Sm, Sb, Sb, Sb]
            ],
            [ -- 8
                [Sb, Mn, Mn, Sb, Mn, Sb, Mn, Sb, Sb],
                [Sb, Mn, Mn, Sb, Mn, Mn, Sb, Sb, Sb],
                [Sb, Mn, MnD, Fs, Fs, Mn, SbD, Sb, Mn, Sb], -- XXX unclear
                [Sb, Mn, Mn, Sb, Sb, MnD, Sm, Sb, Sb]
            ],
            [ -- 9
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = Column c2v c2r

-- {{{2 c2v
c2v = buildVpermTable 
    [ 
        [ -- 0
            [3, 2, 1, 7, 1],
            [1, 7, 6, 5, 6, 5, 6],
            [3, 3, 3, 3, 3],
            [6, 5, 4, 3, 6]
        ],
        [ -- 1
            [3, 3, 4, 3, 3],
            [6, 6, 6, 5, 6, 5, 6],
            [8, 8, 2, 2, 8, 7, 8],
            [6, 6, 2, 3, 6]
        ],
        [ --2
            [5, 5, 5, 4, 3, 2, 8, 2, 8, 2],
            [7, 7, 7, 6, 6],
            [2, 2, 2, 5, 4, 3, 4],
            [5, 5, 5, 6, 2]
        ],
        [ --3
            [2, 2, 3, 2, 2],
            [7, 7, 8, 8, 7, 6, 7],
            [5, 5, 5, 6, 4, 5, 4, 5],
            [5, 5, 1, 2, 5]
        ],
        [ --4
            [8, 8, 2, 8, 7, 8],
            [6, 6, 6, 7, 5, 6, 5, 6],
            [3, 3, 4, 3, 3],
            [6, 6, 2, 3, 6]
        ],
        [ -- 5
            [4, 5, 5, 6, 5],
            [8, 8, 8, 8, 8],
            [6, 5, 5, 4, 3],
            [4, 3, 3, 4, 1]
        ],
        [ --6
            [7, 7, 8, 8, 7, 6, 7],
            [5, 5, 5, 6, 4, 5, 4, 5],
            [2, 2, 3, 6, 7, 8, 2, 2],
            [5, 5, 1, 2, 5]
        ],
        [ -- 7
            [2, 1, 7, 6, 7, 2, 2],
            [7, 6, 5, 4, 5, 4, 5, 4, 5],
            [2, 2, 1, 7, 6, 7],
            [5, 4, 3, 2, 5]
        ],
        [ -- 8
            [3, 3, 3, 4, 5, 4, 5],
            [8, 8, 8, 8, 7, 8, 2, 2],
            [5, 5, 5, 6, 5, 6, 7],
            [1, 1, 1, 2, 5]
        ],
        [ --9
            [8, 8, 8, 2, 7, 1, 7, 1],
            [6, 6, 6, 5, 5],
            [4, 4, 4, 4, 3, 2, 3],
            [4, 4, 4, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c2r
c2r = buildRpermTable 
    [ 
        [ -- duple 
            [ -- 0
                [SbR, SbD, Sm, Sm, Sb, Sb],
                [SbD, Sm, Sm, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Br, Sb],
                [SbD, Sm, Sm, Br, Sb]
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
                [MnD, Sm, SbD, Sm, Sb, Sb],
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
                [MnD, Sm, Sb, Mn, Sm, Sm, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 7
                [SbR, SbD, Sm, Sm, Sm, Sm, Mn, Sb],
                [SbD, Sm, Sm, Mn, Mn, Sm, Mn, Sm, Sb],
                [BrR, Sb, Sm, Fs, Fs, Mn, Sb],
                [SbD, Sm, Sm, Br, Sb]
            ],
            [ -- 8
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [SmD, Sm, Sb, Mn, Sm, Sm, Sb, Sb],
                [MnD, Sm, Mn, Sb, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 9
                [SmR, Mn, Sm, Mn, Mn, Sm, Mn, Sm, Sb],
                [SmR, Mn, Sm, Sb, Sb, Sb],
                [SmR, Mn, Sm, Mn, MnD, Sm, Mn, Sb],
                [SmR, Mn, Sm, Sb, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = Column c3v c3r

-- {{{2 c3v
c3v = buildVpermTable 
    [ 
        [ -- 0
            [3, 5, 6, 2, 3, 2, 3],
            [8, 2, 8, 7, 8, 7, 8],
            [5, 5, 5, 4, 5, 5],
            [8, 7, 6, 5, 1]
        ],
        [ -- 1
            [2, 2, 2, 3, 8, 2, 8, 2],
            [7, 7, 7, 6, 6],
            [5, 5, 5, 5, 4, 3, 4],
            [5, 5, 5, 6, 2]
        ],
        [ --2
            [3, 3, 3, 3, 3],
            [1, 7, 6, 5, 6, 5, 6],
            [3, 2, 8, 7, 8],
            [6, 5, 4, 3, 6]
        ],
        [ --3
            [3, 5, 5, 4, 3, 2, 3],
            [8, 2, 8, 7, 1, 7, 1],
            [5, 2, 3, 8, 2, 3, 4, 5, 5],
            [8, 7, 6, 5, 1]
        ],
        [ --4
            [6, 5, 5, 4, 3],
            [8, 8, 8, 8, 8],
            [4, 3, 3, 6, 5],
            [4, 8, 8, 4, 1]
        ],
        [ -- 5
            [3, 4, 5, 5, 4, 3, 2, 3],
            [8, 2, 3, 2, 8, 7, 1, 7, 1],
            [5, 5, 5, 5, 5],
            [8, 7, 6, 5, 1]
        ],
        [ --6
            [5, 5, 6, 5, 5],
            [8, 8, 8, 2, 7, 1, 7, 1],
            [3, 3, 4, 4, 3, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 7
            [8, 2, 3, 4, 3, 2, 8, 7, 8, 7, 8],
            [6, 6, 6, 5, 5],
            [4, 4, 3, 2, 8, 4, 3, 2, 3],
            [4, 4, 4, 5, 1]
        ],
        [ -- 8
            [3, 4, 5, 3, 6, 5, 4, 3, 2, 3],
            [8, 2, 3, 8, 8, 8, 8, 2, 7, 1, 7, 1],
            [5, 4, 3, 6, 5, 5],
            [8, 4, 5, 6, 7, 8, 4, 5, 1]
        ],
        [ --9
            [6, 6, 5, 6, 5, 4, 5],
            [8, 8, 8, 8, 8],
            [4, 4, 3, 4, 5, 3, 4, 1, 2, 3, 4, 1, 4, 4, 3, 2, 3],
            [4, 4, 8, 4, 1]
        ]
    ]
-- }}}2
-- {{{2 c3r
c3r = buildRpermTable 
    [ 
        [ -- duple 
            [ -- 0
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sb, Mn, Sb, Mn, Sb],
                [MnD, Sm, Mn, Mn, Br, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 1
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 2
                [Sb, Mn, Mn, Br, Sb],
                [SbD, Sm, Sm, Mn, Sb, Mn, Sb],
                [SbR, SbD, Sm, Sm, Sb, Sb],
                [SbD, Sm, Sm, Br, Sb]
            ],
            [ -- 3
                [MnD, Sm, MnD, Mn, Sm, Mn, Sb],
                [MnD, Sm, Sb, Sm, Mn, Sm, Sb],
                [MnD, Sm, Mn, Mn, Sm, Fs, Fs, Mn, Sb],
                [MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 4
                [Sm, Fs, Fs, Mn, Sb],
                [Sm, Fs, Fs, Mn, Sb],
                [Sm, Fs, Fs, Mn, Sb],
                [Sm, Fs, Fs, Mn, Sb] -- not florid
            ],
            [ -- 5  
                [SmD, Fs, Mn, Sm, Fs, Fs, Mn, Sb],
                [SmD, Fs, Sm, Fs, Fs, Sm, Mn, Sm, Sb],
                [Mn, Sm, Sm, Sb, Sb],
                [MnD, Fs, Fs, Sb, Sb] -- Fusae notated differently than others
            ],
            [ -- 6
                [MnD, Sm, Sb, Br, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [MnD, Sm, Sb, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sb, Br, Sb]
            ],
            [ -- 7
                [MnD, Fs, Fs, Sm, Sm, Sm, Sm, Sm, Mn, Sm, Sb],
                [MnD, Sm, Sb, Sb, Sb],
                [MnD, Fs, Fs, Sm, Sm, MnD, Sm, Mn, Sb],
                [MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 8
                [Sm, Sm, Sm, Sm, Sb, Mn, MnD, Sm, Mn, Sb],
                [Sm, Sm, Sm, Sm, Sb, Mn, Sm, Sm, Sm, Mn, Sm, Sb],
                [Sb, Sb, Mn, Mn, Sb, Sb],
                [Sb, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 9
                [SmR, Mn, Sm, Sb, SbD, Sm, Sm, Sb],
                [SmR, Mn, Sm, Sb, Br, Sb],
                [SmR, Mn, Sm, SmD, Fs, Sm, Sm, Fs, Fs, Fs, Fs, Fs, Fs, Mn, Sm, Sm, Sm, Sb],
                [SmR, Mn, Sm, Sb, Br, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1

