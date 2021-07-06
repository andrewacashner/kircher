{-|
Module      : Arca_musarithmica.Syntagma2.Pinax05
Description : Data for the ark, syntagma 2, pinax 5 (Enneasyllabic & Decasyllabic)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p5') contains four columns for successive strophes. Each
contains pairs of four-voice vperms and four-voice rperms.

Kircher intends this for meters with nine and ten syllable lines (p. 116),
with the paradigm being /Amant venena parricida/.

> "PINAX V. Melothesiae [-as?] Floridae & artificiosae Musarithmos continens. Pro metris Enneasyllabis & Decasyllabis."

This pinax is for /toni/ (modes) 1, 2, 3, 4, 9, and 10.
-}

module Arca_musarithmica.Syntagma2.Pinax05 (s2p5) where

import Aedifico
    (
        Dur (..),
        buildColumn,
        buildPinax
    )

-- | Pinax 5
s2p5 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = (c0v, c0r)

-- {{{2 c0v
c0v = [ 
        [ -- 0
            [7, 1, 3, 2, 2, 1, 2, 3, 2, 3],
            [5, 5, 5, 5, 4, 3, 4, 5, 6, 5, 5],
            [2, 1, 1, 1, 7, 1, 1, 1, 7, 6, 7, 1],
            [5, 3, 1, 5, 1, 8, 4, 5, 1]
        ],
        [ -- 1
            [3, 4, 5, 4, 3, 2, 1, 7, 6, 7, 1],
            [5, 6, 5, 1, 7, 6, 5, 5, 4, 5, 5],
            [1, 1, 1, 1, 7, 1, 2, 3, 2, 1, 2, 3],
            [1, 4, 3, 4, 1, 5, 6, 5, 1]
        ],
        [ --2
            [1, 1, 7, 1, 7, 6, 7, 1, 7, 6, 7, 1],
            [5, 6, 5, 4, 3, 4, 5, 4, 5, 5, 5],
            [3, 4, 2, 1, 3, 1, 2, 3, 4, 3, 2, 3],
            [1, 4, 5, 6, 3, 4, 1, 5, 1]
        ],
        [ --3
            [2, 5, 4, 3, 2, 3, 2, 1, 1, 7, 1],
            [7, 1, 2, 1, 7, 1, 7, 6, 5, 5, 5],
            [5, 5, 5, 5, 5, 4, 3, 2, 3],
            [5, 6, 7, 8, 5, 3, 4, 1, 5, 1]
        ],
        [ --4
            [3, 4, 7, 1, 3, 2, 1, 7, 1],
            [5, 6, 4, 5, 5, 4, 5, 5],
            [1, 1, 2, 1, 5, 1, 6, 1, 2, 3, 2, 3],
            [1, 4, 2, 3, 4, 5, 6, 5, 1]
        ],
        [ -- 5
            [3, 4, 4, 3, 3, 3, 2, 2, 3],
            [5, 6, 7, 1, 1, 1, 1, 7, 6, 7, 1],
            [1, 1, 2, 5, 5, 5, 6, 6, 5, 5],
            [1, 4, 2, 3, 1, 8, 4, 5, 1]
        ],
        [ --6
            [5, 5, 4, 5, 4, 3, 3, 2, 3],
            [1, 1, 1, 1, 7, 7, 6, 5, 5],
            [3, 3, 2, 1, 3, 2, 1, 7, 1, 7, 1],
            [8, 6, 6, 5, 4, 3, 4, 5, 1]
        ],
        [ -- 7
            [5, 6, 5, 1, 1, 3, 2, 1, 7, 1],
            [3, 4, 5, 6, 5, 5, 4, 5, 5],
            [1, 1, 1, 1, 7, 1, 2, 3, 2, 3],
            [1, 4, 3, 4, 1, 5, 6, 5, 1]
        ],
        [ -- 8
            [7, 2, 1, 7, 1, 6, 7, 1, 2, 3, 2, 3],
            [5, 5, 5, 5, 5, 4, 3, 4, 5, 5],
            [3, 2, 3, 2, 3, 1, 7, 1, 7, 1],
            [3, 7, 1, 5, 3, 4, 1, 5, 1]
        ],
        [ --9
            [7, 1, 2,3 , 4, 1, 1, 2, 4, 3, 4, 5, 5, 4, 5, 5],
            [5, 7, 6, 5, 4, 2, 5, 6, 7, 1, 7, 1],
            [2, 7, 1, 1, 7, 6, 7, 1, 2, 3, 2, 1, 2, 3],
            [5, 4, 3, 2, 1, 8, 6, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r = [ 
        [ -- duple 
            [ -- 0
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Mn, Sb, Sb],
                [Sb, Mn, Mn, MnD, Sm, Sm, Sm, Mn, Mn, Mn, Sb], -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sm, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Sb, MnD, Sm, Mn, Mn, Sb]
            ],
            [ -- 1
                [Sb, Mn, MnD, Mn, Sm, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Sm, Fs, Fs, Mn, Mn, MnD, Sm, Sb, Sb],
                [Sb, Mn, Mn, Sb, Mn, Sm, Sm, Sm, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [Sb, Mn, Mn, MnD, Sm, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Sm, Sm, Sm, Sm, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Sm, Fs, Fs, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 3
                [Mn, MnD, Sm, Mn, Mn, Mn, Sm, Sm, Sb, Mn, Sb],
                [MnD, Sm, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb], -- unclear
                [MnD, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
                    -- correction: 
                    --  original MnD Mn Mn Mn Mn Mn Mn Sb Sb
            ],
            [ -- 4
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Sb, MnD, Sm, Sb, Sb],
                [Sb, Mn, Mn, Sm, Sm, Mn, Mn, Sm, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 5
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb], -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Sb]
            ],
            [ -- 6
                [Sb, Mn, Mn, SbD, Mn, Sm, Sm, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Sm, Sm, Mn, Mn, Sb, Sm, Mn, Sm, Sb], -- unclear
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 7
                [Sb, Mn, Sm, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb, Sb],
                [Sb, Mn, Mn, Sb, Mn, Sm, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 8
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Sm, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sm, Mn, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 9
                [Sm, Sm, Sm, Sm, Sm, Mn, Sm, MnD, Sm, Sm, Sm, MnD, Sm, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Mn, Mn, MnD, Fs, Fs, SbD, Mn, Sb],
                [Mn, Mn, Mn, MnD, Fs, Fs, Mn, Sm, Sm, Sb, Sm, Sm, Sb, Sb],
                [Sb, Mn, Mn, Sb, Mn, Mn, Sb, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c1
c1 = (c1v, c1r)

-- {{{2 c1v
c1v = [ 
        [ -- 0
            [3, 2, 1, 1, 7, 1, 1, 2, 3, 4, 5, 4, 3, 2, 1, 2, 3],
            [1, 7, 6, 5, 6, 5, 5, 5, 6, 7, 7, 6, 5, 6, 7, 7],
            [5, 4, 3, 2, 2, 3, 3, 4, 3, 2, 3, 4, 5],
            [1, 2, 3, 4, 5, 1, 8, 7, 3, 7, 8, 7, 3]
        ],
        [ -- 1
            [7, 1, 3, 3, 4, 5, 5, 5, 5],
            [5, 5, 5, 6, 7, 1, 1, 1, 7, 1],
            [2, 1, 1, 2, 3, 2, 1, 3, 3, 2, 3],
            [5, 3, 1, 8, 6, 5, 5, 5, 1]
        ],
        [ --2
            [7, 1, 2, 1, 7, 7, 6, 7, 6, 7],
            [5, 5, 5, 5, 4, 5, 4, 5, 5, 4, 5],
            [2, 3, 2, 3, 2, 1, 2, 2, 2, 2, 2],
            [5, 8, 7, 6, 5, 2, 7, 1, 2, 5]
        ],
        [ --3
            [2, 3, 5, 5, 4, 5, 2, 3, 2, 2],
            [7, 1, 2, 1, 7, 7, 6, 6, 6, 7],
            [5, 5, 2, 3, 2, 1, 2, 5, 4, 5, 4, 5],
            [5, 8, 7, 6, 5, 2, 1, 2, 5]
        ],
        [ --4
            [1, 7, 1, 2, 3, 2, 3, 3, 2, 3],
            [5, 5, 5, 6, 7, 7, 6, 5, 6, 7, 7],
            [3, 2, 3, 4, 5, 4, 3, 4, 5],
            [1, 2, 3, 4, 5, 1, 8, 7, 3, 7, 1, 7, 3]
        ],
        [ -- 5
            [1, 7, 1, 2, 4, 3, 1, 1, 7, 1],
            [5, 5, 5, 4, 2, 5, 4, 5, 5],
            [3, 2, 1, 1, 7, 1, 2, 3, 2, 3],
            [8, 5, 3, 2, 2, 1, 6, 5, 1]
        ],
        [ --6
            [1, 1, 7, 1, 7, 6, 7, 1, 6, 5, 6],
            [5, 5, 5, 4, 5, 4, 5, 6, 4, 3, 2, 3, 4],
            [3, 4, 2, 1, 3, 1, 1, 1, 1],
            [8, 4, 5, 6, 3, 4, 4, 1, 4]
        ],
        [ -- 7
            [2, 3, 4, 5, 4, 5, 7, 6, 6, 6, 5],
            [7, 1, 2, 1, 7, 2, 2, 3, 2, 7],
            [5, 3, 3, 2, 1, 2, 5, 4, 5, 5, 4, 5],
            [5, 8, 7, 6, 5, 2, 1, 2, 5]
        ],
        [ -- 8
            [7, 1, 7, 7, 6, 7, 7, 2, 1, 2],
            [5, 5, 4, 4, 4, 5, 4, 4, 4],
            [2, 3, 2, 1, 2, 7, 7, 6, 7],
            [5, 3, 7, 4, 2, 3, 7, 4, 7]
        ],
        [ --9
            [1, 1, 7, 1, 7, 6, 7, 1, 2, 3, 4, 3, 4],
            [5, 6, 5, 4, 5, 4, 5, 6, 5, 4, 5, 6, 5, 6],
            [3, 4, 2, 1, 3, 1, 1, 1, 1],
            [6, 4, 5, 6, 3, 4, 3, 2, 1, 4]
        ]
    ]
-- }}}2
-- {{{2 c1r
c1r = [ 
        [ -- duple 
            [ -- 0
                [MnD, Sm, Sm, Sm, Mn, Mn, Mn, Sm, Fs, Fs, Mn, Mn, MnD, Fs, Fs, Mn, Sb],
                    -- correction: value 1 was Mn
                [MnD, Fs, Fs, Sm, Sm, Mn, Mn, Sm, Sm, Mn, MnD, Sm, Sm, Sm, Sb, Sb],
                [MnD, Sm, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [Sb, Mn, Mn, MnD, Sm, Sm, Sm, Mn, Sb],
                [Sb, Mn, MnD, Fs, Fs, Mn, Sm, Sm, Mn, Sb],
                [Sb, Mn, Sm, Sm, MnD, Fs, Fs, Sm, Sm, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Mn, Sb]
            ],
            [ -- 2
                [Sb, MnD, Sm, Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Sb],
                [Sb, MnD, Sm, Mn, Sm, Sm, Mn, Sb, Mn, Sb, Sb],
                [Sb, MnD, Sm, Sb, Sb, Mn, Sm, Sm, Sb, Sb]
            ],
            [ -- 3
                [MnR, Mn, Sm, Sm, Sm, Sm, MnD, Mn, Sm, Mn, Sb],
                [Sb, Sm, Sm, Mn, MnD, Sm, Sm, Sm, Mn, Sb],
                [Sb, Sm, Sm, Sm, Fs, Fs, Mn, Mn, Sm, Mn, Sm, Sb], -- unclear
                [Sb, Sm, Sm, Mn, Sb, Sm, Sm, Mn, Sb]
            ],
            [ -- 4
                [SbD, Mn, Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, MnD, Sm, Mn, MnD, Sm, Sm, Sm, Sb, Sb],
                [SbD, Mn, MnD, Sm, Sb, Mn, Mn, Mn, Mn, Sb], 
                    -- correction: I added value 3
                    -- something is wrong! XXX
                [MnD, Sm, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 5
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [Sb, Mn, Mn, MnD, Sm, Sm, Sm, Sm, Sm, Sb, Sb], -- unclear
                [Sb, Mn, MnD, Sm, Mn, Sm, Sm, Sm, Mn, Fs, Fs, Mn, Sb], -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb], -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 7
                [Sb, Sm, Sm, Sb, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Sb, MnD, Sm, Mn, Mn, Sb, Sb],
                [MnR, Sb, Mn, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, MnD, Sm, Sb, Sb, Mn, Mn, Sb, Sb]
            ],
            [ -- 8
                [Sb, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 9
                [Sb, Mn, Mn, MnD, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Sm, Sb],
                [Sb, Mn, MnD, Sm, Mn, Sm, Sm, Sm, Fs, Fs, Sm, Sm, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Fs, Fs, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = (c2v, c2r)

-- {{{2 c2v
c2v = [ 
        [ -- 0
            [7, 1, 2, 1, 1, 7, 2, 2, 1, 2, 2],
            [5, 5, 5, 5, 4, 5, 4, 5, 4, 5],
            [2, 3, 2, 3, 2, 1, 2, 7, 6, 5, 7, 6, 7],
            [5, 8, 7, 6, 5, 2, 3, 2, 5]
        ],
        [ -- 1
            [7, 7, 2, 1, 7, 1, 2, 2, 3, 2, 3],
            [5, 5, 7, 5, 5, 4, 5, 4, 5, 5, 6, 5, 5],
            [2, 3, 4, 3, 2, 1, 7, 6, 7, 1, 7, 1],
            [5, 3, 7, 1, 2, 3, 4, 5, 3, 2, 5, 3, 4, 5, 1]
        ],
        [ --2
            [1, 1, 1, 7, 1, 6, 7, 1, 7, 1],
            [5, 5, 4, 5, 5, 4, 3, 4, 5, 5],
            [3, 3, 2, 1, 2, 3, 1, 2, 3, 2, 3],
            [8, 6, 6, 5, 3, 4, 6, 5, 1]
        ],
        [ --3
            [5, 6, 5, 3, 5, 4, 3, 2, 3],
            [1, 1, 7, 1, 7, 6, 5, 5, 5],
            [3, 4, 2, 1, 1, 1, 7, 1, 1],
            [1, 4, 5, 6, 3, 4, 1, 5, 1]
        ],
        [ --4
            [2, 3, 4, 5 ,4, 5, 2, 3, 2, 2],
            [7, 1, 1, 7, 7, 6, 7, 1, 7, 7, 6, 7],
            [2, 5, 3, 2, 1, 2, 5, 4, 5, 5, 4, 5],
            [5, 8, 6, 5, 5, 2, 1, 2, 5]
        ],
        [ -- 5
            [2, 3, 2, 1, 2, 7, 7, 6, 7],
            [5, 5, 4, 4, 4, 5, 4, 4, 4],
            [7, 7, 7, 6, 7, 7, 2, 1, 2],
            [5, 3, 7, 4, 2, 3, 7, 4, 7]
        ],
        [ --6
            [7, 3, 3, 4, 5, 4, 3, 1, 7, 6, 7, 1],
            [5, 5, 5, 6, 2, 5, 4, 5, 5], -- 2 unclear
            [2, 1, 7, 1, 2, 3, 2, 1, 2, 3],
            [5, 3, 3, 2, 2, 1, 6, 5, 1]
        ],
        [ -- 7
            [3, 4, 5, 4, 3, 2, 3, 2, 5, 4, 2, 3, 2, 3], --unclear
            [1, 7, 6, 5, 6, 5, 5, 6, 7, 1, 7, 7, 7],
            [5, 4, 3, 1, 2, 7, 1, 7, 1, 2, 3, 4, 3, 6, 5, 4, 5],
            [1, 2, 3, 4, 5, 1, 8, 7, 3, 6, 7, 7, 3]
        ],
        [ -- 8
            [5, 6, 5, 5, 4, 4, 3, 2, 3],
            [1, 1, 7, 1, 7, 1, 1, 7, 1],
            [3, 4, 2, 1, 3, 2, 1, 2, 3, 4, 5, 5],
            [1, 4, 5, 6, 6, 5, 5, 5, 1]
        ],
        [ --9
            [2, 3, 2, 1, 2, 1, 7, 7, 6, 5, 6, 7],
            [7, 7, 7, 6, 7, 5, 3, 4, 4],
            [5, 5, 4, 4, 4, 3, 2, 1, 7, 1, 2],
            [5, 3, 7, 4, 7, 3, 5, 4, 7]
        ]
    ]
-- }}}2
-- {{{2 c2r
c2r = [ 
        [ -- duple 
            [ -- 0
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Sb, Sb, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [Mn, Sb, Mn, Sb, Mn, Mn, Sb, Mn, Mn, Sb, Sb],
                [Mn, Sb, Mn, Sb, Sb, Sm, Mn, Sm, Mn, Mn, Mn, Mn, Sb],
                [Mn, Sb, Mn, Sb, Mn, MnD, Sm, Mn, Mn, Sb, Mn, Sb],
                [Mn, Sb, Mn, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 2
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Sb],
                [Sb, Mn, Sm, Sm, Mn, Mn, MnD, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 3
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, SbD, Mn, Mn, Mn], -- error?
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 4
                [Sb, Sm, Sm, Sb, Mn, Mn, Sb, Mn, Sb, Sb],
                [Sb, Sb, Sb, Mn, Mn, Sm, Sm, Sm, Sm, Mn, Mn, Sb],
                [Mn, Sb, Sb, Sm, Sm, MnD, Sm, Mn, Mn, Mn, Mn, Sb],
                [Sb, Sb, Sb, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 5  
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [Sb, Mn, Mn, MnD, Fs, Fs, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Sb],
                [SbD, Sb, Mn, MnD, Sm, Sm, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 7
                [MnD, Sm, MnD, Mn, Sm, Mn, Sb, Mn, Sb, Mn, Mn, Sb, Mn, Sb],
                [MnD, Mn, Fs, Fs, Mn, Sb, MnD, Sm, Sb, Sb, Sb, Sb, Sb],
                [MnD, Sm, Mn, Sm, Sm, Sm, Mn, Sm, Sm, Sm, Mn, Mn, Sb, Sb, Mn, Sb, Sb],
                    -- correction? dot added to value 1
                [MnD, Sm, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Sb, Sb, Sb, Sb]
            ],
            [ -- 8
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Sm, Sm, Sm, Sm, Mn, Sm, Sm, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 9
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Sm, Sm, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = (c3v, c3r)

-- {{{2 c3v
c3v = [ 
        [ -- 0
            [7, 7, 7, 1, 2, 7, 1, 7, 1],
            [5, 5, 5, 6, 6, 5, 5, 5, 5],
            [3, 3, 3, 3, 4, 4, 3, 2, 3],
            [3, 3, 3, 6, 4, 5, 5, 5, 1]
        ],
        [ -- 1
            [5, 5, 5, 3, 4, 4, 3, 2, 3],
            [7, 7, 7, 1, 1, 7, 1, 1, 7, 1],
            [3, 3, 3, 3, 1, 2, 3, 4, 5, 5],
            [3, 3, 3, 6, 6, 5, 5, 5, 1]
        ],
        [ --2
            [7, 1, 2, 1, 7, 1, 1, 7, 6, 7, 1],
            [5, 6, 5, 4, 4, 5, 4, 5, 5],
            [2, 6, 7, 6, 5, 6, 2, 3, 2, 1, 2, 3],
            [5, 4, 3, 2, 2, 1, 6, 5, 1]
        ],
        [ --3
            [5, 6, 5, 3, 5, 4, 3, 4, 5, 6, 6, 5, 4, 5],
            [1, 1, 7, 1, 7, 6, 5, 6, 7, 1, 1, 1],
            [3, 4, 2, 1, 1, 1, 4, 4, 3, 2, 3],
            [8, 4, 5, 6, 3, 4, 1, 4, 1]
        ],
        [ --4
            [8, 4, 4, 3, 3, 3, 2, 1, 7, 1, 7, 1],
            [5, 6, 7, 1, 1, 1, 6, 5, 5],
            [1, 1, 2, 5, 5, 5, 4, 3, 2, 3],
            [1, 4, 2, 3, 1, 8, 4, 5, 1]
        ],
        [ -- 5
            [7, 2, 1, 7, 1, 6, 7, 1, 7, 6, 7, 1],
            [5, 5, 5, 5, 5, 4, 5, 5, 5],
            [3, 2, 3, 2, 3, 1, 3, 2, 3],
            [3, 7, 1, 5, 3, 4, 1, 5, 1]
        ],
        [ --6
            [5, 6, 5, 3, 5, 4, 3, 4, 1, 4, 3, 2, 3],
            [1, 1, 7, 1, 7, 6, 5, 6, 5, 4, 5],
            [3, 4, 2, 1, 3, 1, 1, 1, 1],
            [8, 4, 5, 6, 3, 4, 1, 4, 1]
        ],
        [ -- 7
            [7, 7, 2, 1, 7, 3, 2, 7, 1, 7, 1],
            [5, 5, 7, 5, 5, 5, 6, 5, 5],
            [2, 3, 4, 3, 2, 1, 4, 3, 2, 3],
            [5, 3, 7, 1, 5, 3, 4, 5, 1]
        ],
        [ -- 8
            [3, 4, 5, 5, 4, 4, 3, 2, 3, 3],
            [5, 8, 7, 6, 6, 6, 5, 4, 5, 5],
            [8, 8, 8, 8, 8, 8, 8, 8, 8],
            [1, 6, 3, 4, 4, 1, 1, 1, 1]
        ],
        [ --9
            [1, 1, 7, 7, 6, 4, 2, 3, 2, 3],
            [5, 6, 5, 5, 4, 5, 5, 6, 5, 5],
            [3, 2, 2, 1, 2, 7, 1, 7, 6, 7, 1],
            [8, 4, 5, 3, 4, 3, 4, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c3r
c3r = [ 
        [ -- duple 
            [ -- 0
                [Mn, Sm, Sm, MnD, Sm, Mn, Sb, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [Mn, Sm, Sm, SbD, Mn, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, SbD, Mn, Mn, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [Sb, Mn, Sb, Sb, Mn, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, Sb, Sb, Mn, Mn, MnD, Sm, Sb, Sb],
                [Sb, Sb, Mn, Sm, Sm, Mn, Mn, MnD, Fs, Fs, Sb, Sb],
                [SbD, Mn, Sb, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 3
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sm, Sm, MnD, Sm, Sm, Sm, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, Sm, Sm, Sb, Mn, Sb],
                [Sb, Mn, Mn, Sb, Sb, MnD, Mn, Sm, Sm, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Br]
            ],
            [ -- 4
                [Sb, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Sm, Mn, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 5  
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                    -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                    -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [Mn, Mn, Sm, Mn, Sm, Mn, Mn, Sm, Sm, Sb, Sm, Sm, Sb],
                [Mn, Mn, Sm, Mn, Sm, Mn, Mn, SbD, Sm, Sm, Sb],
                    -- unclear
                [Mn, Mn, Sm, Mn, Sm, Sb, Sb, Sb, Sb],
                [Mn, Mn, Sm, Mn, Sm, Mn, Mn, Sb, Br] 
                    -- unclear !
            ],
            [ -- 7
                [Mn, Sb, Mn, Mn, Sb, Sb, Mn, Mn, Sb, Mn, Sb],
                [Mn, Sb, Mn, Mn, Sb, Mn, Sb, Br, Sb],
                [Mn, Sb, Mn, Mn, Sb, Sb, Sb, Mn, Sb, Sb],
                [Mn, Sb, Mn, Mn, Sb, Mn, Sb, Br, Sb] -- unclear!
            ],
            [ -- 8
                [Sb, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Mn, Mn],
                [Sb, MnD, Sm, Mn, Mn, Mn, Sm, Sm, Mn, Mn],
                    -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn] -- unclear
            ],
            [ -- 9
                [Sb, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Sb, Sb], -- unclear
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Sb, Mn, Mn, Mn, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1

