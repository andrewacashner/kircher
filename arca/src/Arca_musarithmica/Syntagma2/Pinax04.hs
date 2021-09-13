{-|
Module      : Arca_musarithmica.Syntagma2.Pinax04
Description : Data for the ark, syntagma 2, pinax 4 (Iambic Archilocical)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p4') contains four columns for successive strophes. Each
contains pairs of four-voice vperms and four-voice rperms.

Kircher intends this for "pre-iambic archilochical" meters (p. 111), with the paradigm being /Veni creator Spiritus/.

> "PINAX IV. Melothesiae [-as?] floridae & artificiosae Musarithmos continens PreIambicis Archilochijs."

This pinax is for these /toni/ (tones): 

* strophes 1--2: 5, 6, 7, 8, 11, and 12
* strophes 3--4: 1, 2, 3, 4, 9, 10 

(really?)
-}

module Arca_musarithmica.Syntagma2.Pinax04 (s2p4) where

import Aedifico
    ( Dur (..)
    , buildPinax
    )

-- | Pinax 4
s2p4 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = (c0v, c0r)

-- {{{2 c0v
c0v = [ 
        [ -- 0
            [1, 7, 2, 3, 3, 2, 2, 3],
            [5, 5, 6, 7, 8, 5, 1, 1, 7, 1],
            [3, 2, 4, 5, 5, 5, 5, 5],
            [1, 5, 4, 3, 2, 1, 5, 5, 1]
        ],
        [ -- 1
            [1, 5, 3, 4, 5, 6, 6, 5, 4, 3, 2, 3, 2, 3, 4, 3, 2, 1, 1, 7, 6, 7, 1],
            [5, 8, 6, 7, 1, 4, 4, 3, 2, 1, 7, 6, 7, 1, 7, 1, 2, 1, 6, 4, 5, 5],
            [1, 5, 3, 4, 5, 3, 6, 5, 4, 3, 2, 8, 2, 3],
            [5, 8, 6, 7, 8, 4, 6, 5, 1]
        ],
        [ --2
            [5, 5, 5, 4, 3, 2, 2, 3],
            [8, 7, 8, 8, 8, 8, 7, 8],
            [3, 2, 3, 6, 5, 5, 5, 5],
            [1, 5, 3, 4, 1, 5, 5, 1]
        ],
        [ --3
            [3, 3, 3, 2, 3, 2, 1, 1, 7, 1],
            [1, 1, 1, 7, 1, 6, 5, 5],
            [5, 5, 5, 5, 5, 4, 3, 2, 1, 2, 3],
            [8, 8, 8, 5, 3, 4, 5, 1]
        ],
        [ --4
            [3, 2, 4, 3, 3, 2, 2, 3],
            [8, 7, 2, 1, 1, 1, 7, 1],
            [5, 5, 6, 5, 5, 5, 5, 5],
            [1, 5, 2, 3, 4, 5, 5, 1]
        ],
        [ -- 5
            [5, 5, 5, 4, 3, 2, 2, 3],
            [8, 8, 8, 7, 7, 6, 6, 5, 3, 6, 6, 6, 5, 5, 5],
            [5, 5, 5, 4, 4, 3, 3, 2, 3, 2, 8, 2, 2, 1, 7, 1],
            [8, 8, 8, 7, 7, 6, 2, 8, 6, 5, 4, 3, 2, 3, 4, 5, 6, 3, 5, 5, 1]
        ],
        [ --6
            [3, 4, 3, 4, 5, 5, 4, 5],
            [1, 1, 1, 2, 2, 1, 1, 7],
            [5, 6, 5, 2, 2, 3, 2, 8, 2],
            [8, 4, 8, 8, 7, 6, 6, 5]
        ],
        [ -- 7
            [2, 3, 4, 4, 3, 2, 2, 3], -- value 1 unclear
            [7, 1, 1, 2, 1, 1, 7, 1],
            [5, 5, 6, 5, 5, 5, 5, 5],
            [5, 8, 6, 7, 8, 5, 5, 1]
        ],
        [ -- 8
            [1, 2, 2, 3, 1, 4, 3, 2, 1, 1, 7, 1],
            [5, 6, 6, 7, 5, 8, 6, 5, 5, 6, 6, 5, 5],
            [8, 2, 2, 2, 8, 2, 3, 4, 3, 2, 3, 5, 3, 4, 4, 3, 2, 4, 3, 2, 2, 3],
            [5, 6, 6, 7, 7, 6, 7, 1, 1, 5, 6, 7, 7, 1, 6, 4, 5, 5, 1]
        ],
        [ --9
            [5, 4, 4, 4, 3, 2, 2, 3],
            [2, 1, 1, 2, 1, 1, 7, 1],
            [7, 6, 6, 5, 5, 6, 5, 5],
            [5, 6, 6, 7, 6, 4, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r = [ 
        [ -- duple 
            [ -- 0
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Sm, Sm, Sm, Sm, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, MnD, Sm, Sb]
            ],
            [ -- 1
                [Sb, SbD, Mn, Mn, Mn, MnD, Sm, MnD, Fs, Fs, Sb, Sb, Sb, SbD, Sb, Mn, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                    -- unclear
                [SbR, MnR, Mn, Sb, Mn, Mn, Sm, Sm, Mn, Sm, Sm, MnD, Fs, Fs, Sb, Sb, Mn, Br, Mn, Mn, Mn, Mn, Sb, Sb],
                    -- correction? first two values SbR, MnR
                    -- look like Br
                [LgR, SbR, Sb, SbD, Mn, Mn, Sb, Mn, Sb, Sb, Sm, Sm, Sm, Sm, Sb, Sb],
                [LgR, LgR, MnR, Mn, SbD, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [Sb, SbD, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, SbD, Mn, Mn, Mn, MnD, Sm, Sb]
            ],
            [ -- 3
                [Mn, Sm, Sm, MnD, Sm, Sm, Sm, Sb, Mn, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sb, Sb, Sb],
                [Mn, Sm, Sm, MnD, Sm, MnD, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sb, Sb, Sb]
            ],
            [ -- 4
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, Sb]
            ],
            [ -- 5
                [BrR, SbR, Sm, Fs, Fs, Mn, Mn, SmD, Fs, Sb],
                [SbR, MnR, Sm, Fs, Fs, Sm, Sm, SmD, Fs, Sm, Sm, Sm, Sm, Sm, Sm, SmD, Fs, Sb],
                [MnR, Sm, Fs, Fs, Sm, Sm, SmD, Fs, Sm, Mn, Mn, Sm, Sm, Sm, MnD, Sm, Sb],
                    -- unclear
                [Sm, Fs, Fs, Sm, Sm, SmD, Fs, Sm, Mn, Sm, Mn, Mn, Fs, Fs, Fs, Fs, Sm, Sm, SmD, Fs, Sb]
            ],
            [ -- 6
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 7
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                    -- correction: one Mn removed from series
                    -- in middle
                [MnR, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 8
                [BrR, SbR, Sm, Fs, Fs, Sm, Sm, Fs, Fs, Fs, Fs, Sm, Sm, Sb],
                [BrR, Sm, Fs, Fs, Sm, Sm, Fs, Fs, Sm, Sm, Sm, Mn, Mn, Sb],
                [Sm, Fs, Fs, Sm, Sm, Sm, Sm, Sm, Fs, Fs, Mn, Sm, SmD, Fs, Sm, Mn, Sm, Fs, Fs, Sm, Sm, Sb],
                [MnR, Sm, Fs, Fs, Sm, Sm, Sm, Sm, SmD, Fs, Mn, Sm, Fs, Fs, Sm, Sm, Mn, SmD, Fs, Sb]
            ],
            [ -- 9
                [SmR, Mn, Fs, Fs, Sm, Sm, SmD, Fs, Sb],
                [SmR, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [SmR, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb]
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
            [3, 3, 5, 5, 4, 5, 6, 6, 5],
            [8, 8, 2, 8, 7, 2, 2, 7],
            [5, 5, 2, 3, 2, 1, 2, 3, 4, 5, 4, 5],
            [1, 1, 7, 6, 5, 2, 2, 5]
        ],
        [ -- 1
            [1, 5, 5, 6, 5, 4, 3, 4, 3, 2, 3, 2, 3, 4, 3, 2, 3, 4, 3, 5, 3, 2, 2, 3],
            [4, 8, 8, 2, 1, 7, 1, 7, 1, 7, 6, 5, 1, 6, 7, 1, 2, 1, 7, 1],
            [8, 5, 5, 6, 5, 4, 3, 6, 5, 4, 5, 5, 5, 5],
            [4, 8, 8, 2, 8, 7, 8, 5, 1]
        ],
        [ --2
            [1, 4, 4, 3, 2, 2, 1, 2],
            [6, 6, 6, 6, 5, 6, 6, 6],
            [3, 2, 2, 8, 2, 3, 3, 4],
            [6, 4, 2, 6, 7, 6, 6, 2]
        ],
        [ --3
            [2, 3, 6, 5, 5, 5, 5, 5],
            [7, 1, 1, 2, 1, 1, 7, 1],
            [5, 5, 4, 4, 3, 2, 2, 3],
            [5, 1, 6, 7, 1, 5, 5, 1]
        ],
        [ --4
            [3, 4, 3, 2, 1, 4, 4, 3, 4],
            [8, 8, 7, 6, 8, 8, 8, 8],
            [5, 6, 5, 3, 6, 5, 5, 4],
            [8, 4, 5, 6, 4, 1, 1, 4]
        ],
        [ -- 5
            [5, 5, 5, 8, 7, 6, 6, 7],
            [3, 3, 2, 3, 6, 2, 2, 2, 2],
            [5, 5, 5, 5, 4, 5, 4, 4, 5],
            [8, 8, 7, 6, 5, 2, 2, 5]
        ],
        [ --6
            [3, 3, 5, 5, 4, 5, 2, 3, 2, 2, 2],
            [8, 8, 2, 8, 7, 6, 5, 6, 6, 7],
            [5, 5, 2, 3, 2, 8, 2, 4, 5, 4, 5],
            [1, 1, 7, 6, 5, 4, 3, 2, 1, 2, 2, 5]
        ],
        [ -- 7
            [1, 2, 3, 4, 3, 2, 1, 2, 3, 3, 2, 1, 1, 1, 1, 7, 1],
            [5, 6, 7, 1, 7, 1, 6, 7, 1, 7, 1, 7, 6, 5, 8, 6, 5, 6, 5, 4, 4, 5],
            [8, 2, 3, 4, 3, 2, 8, 2, 3, 4, 5, 4, 3, 5, 4, 3, 1, 1, 2, 2, 3],
            [5, 6, 7, 8, 1, 2, 3, 4, 3, 2, 2, 1]
        ],
        [ -- 8
            [1, 1, 4, 4, 3, 2, 2, 3],
            [5, 6, 6, 7, 1, 1, 7, 1],
            [3, 3, 2, 2, 3, 4, 5, 5],
            [8, 6, 4, 5, 5, 5, 5, 1]
        ],
        [ --9
            [5, 4, 3, 2, 1, 7, 7, 1],
            [8, 7, 6, 5, 6, 7, 5, 5, 5],
            [5, 4, 3, 2, 3, 8, 2, 3, 2, 3, 2, 2, 3],
            [8, 7, 6, 5, 6, 7, 5, 6, 5, 4, 3, 4, 5, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c1r
c1r = [ 
        [ -- duple 
            [ -- 0
                [Sb, MnD, Sm, Mn, Mn, Sb, MnD, Sm, Sb],
                [Sb, MnD, Sm, Sb, Sb, MnD, Sm, Sb],
                [Sb, MnD, Sm, Mn, Sm, Sm, MnD, Sm, Sm, Mn, Sm, Sb],
                [Sb, MnD, Sm, Sb, Sb, MnD, Sm, Sb]
            ],
            [ -- 1
                [Sb, Mn, Mn, MnD, Sm, Sb, Sb, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Sb, Mn, Sb, MnD, Sm, Mn, Mn, MnD, Sm, Sb],
                [BrR, SbR, MnR, Mn, Mn, Mn, MnD, Sm, Mn, Sb, Mn, MnD, Sm, Sb, Mn, Mn, Mn, Sm, Sm, Mn, Sb, Mn, Sb],
                [LgR, BrR, MnR, Mn, Mn, Mn, MnD, Sm, Sb, Mn, Sm, Sm, MnD, Sm, Sb, Sb, Sb],
                [LgR, LgR, SbR, MnR, Mn, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 3
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, SmD, Fs, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb]
            ],
            [ -- 4
                [Sb, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 5
                [Sb, MnD, Sm, Mn, Mn, MnD, Sm, Sb],
                [Sb, MnD, Sm, Sm, Sm, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Sm, Sm, Mn, MnD, Sm, Sb],
                [Sb, MnD, Sm, Mn, Mn, MnD, Sm, Sb]
            ],
            [ -- 6
                [Sb, MnD, Sm, Mn, Mn, Mn, MnD, Sm, SmD, Fs, Sb],
                [Sb, MnD, Sm, Sb, MnD, Fs, Fs, MnD, Sm, Sb],
                [Sb, MnD, Sm, Mn, Sm, Sm, Sb, Sm, Mn, Sm, Sb],
                [Sb, MnD, Sm, Sb, MnD, Fs, Fs, Sm, Sm, Sm, Sm, Sb]
            ],
            [ -- 7
                [LgR, MnR, Mn, Mn, Mn, Sb, Sb, Sb, Mn, Mn, Mn, Mn, Mn, Mn, SbD, Mn, SbD, Mn, Sb],
                [Sb, Mn, Mn, Sb, Mn, Mn, Sb, Mn, Sb, Mn, SbD, Mn, Sb, Mn, Mn, Mn, Mn, SbD, Mn, SbD, Mn, Sb],
                [BrR, MnR, Mn, Mn, Mn, SbD, Mn, Sb, Sm, Sm, Sm, Sm, Sb, Sb, Mn, Mn, Mn, Mn, SbD, Mn, SbD, Mn, Sb],
                [LgR, LgR, MnR, Mn, Mn, Mn, Mn, Mn, Mn, Mn, SbD, Mn, SbD, Mn, Sb]
            ],
            [ -- 8
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 9
                [BrR, Sm, Fs, Fs, Sm, Sm, SmD, Fs, Mn],
                [SbR, MnR, Sm, Fs, Fs, Sm, Sm, Sm, Mn, Sm, Mn],
                [MnR, Sm, Fs, Fs, Sm, Sm, Sm, Sm, Sm, Mn, Sm, Sm, Sm, Mn],
                [Sm, Fs, Fs, Sm, Sm, Sm, Sm, Sm, Fs, Fs, Sm, Sm, MnD, Sm, Mn]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = (c2v, c2r)

-- {{{2 c2v
c2v = 
    [ 
        [ -- 0
            [2, 3, 5, 5, 4, 5, 2, 2, 3, 2, 2, 2],
            [7, 1, 2, 1, 7, 7, 6, 6, 6, 6, 7],
            [5, 5, 2, 3, 2, 1, 2, 5, 4, 3, 4, 4, 5], 
            -- CN? 5, 5, 2, 3, 2, 1, 2, 5, 4, *5, 4, 4, 5],
            [5, 8, 7, 6, 5, 2, 1, 2, 2, 5]
        ],
        [ -- 1
            [7, 1, 2, 1, 7, 7, 6, 6, 6, 6, 7],
            [5, 5, 5, 5, 4, 5, 5, 4, 5, 5, 4, 5],
            [2, 3, 2, 3, 2, 1, 2, 2, 2, 3, 2, 2, 2],
            [5, 8, 7, 6, 5, 5, 2, 1, 2, 2, 5]
        ],
        [ --2
            [5, 5, 5, 3, 4, 5, 5, 5],
            [7, 7, 7, 8, 8, 8, 7, 8],
            [3, 3, 3, 3, 1, 3, 2, 3],
            [3, 3, 3, 6, 6, 5, 5, 1]
        ],
        [ --3
            [3, 2, 7, 1, 7, 6, 7, 1, 7, 6, 7, 1],
            [1, 6, 5, 4, 5, 4, 5, 5, 5, 5],
            [5, 4, 2, 1, 3, 1, 3, 2, 2, 3],
            [1, 4, 5, 6, 3, 4, 1, 5, 5, 1]
        ],
        [ --4
            [3, 4, 3, 2, 3, 2, 8, 8, 7, 6, 7, 1],
            [8, 8, 8, 7, 8, 6, 5, 5],
            [6, 6, 5, 5, 5, 4, 3, 2, 2, 3],
            [6, 4, 1, 5, 3, 4, 1, 5, 5, 1]
        ],
        [ -- 5
            [7, 1, 2, 3, 4, 5, 4, 4, 3, 2, 1, 7, 1],
            [5, 7, 6, 5, 6, 2, 5, 5, 4, 5],
            [2, 7, 1, 1, 7, 6, 7, 1, 2, 3, 4, 3, 2, 1, 2, 2, 3],
            [5, 4, 3, 2, 1, 8, 6, 5, 5, 1]
        ],
        [ --6
            [4, 3, 2, 1, 2, 2, 3, 4, 3, 4, 3, 4],
            [2, 8, 6, 6, 6, 6, 5, 1, 1, 1, 6, 7, 1, 1, 1],
            [6, 5, 4, 3, 4, 4, 5, 6, 5, 4, 5, 5, 6],
            [2, 3, 4, 5, 6, 2, 2, 8, 4, 1, 2, 1, 1, 4]
        ],
        [ -- 7
            [5, 6, 5, 3, 5, 4, 3, 2, 2, 3],
            [8, 8, 7, 8, 7, 6, 5, 5, 5, 5],
            [3, 4, 2, 1, 1, 1, 7, 6, 7, 7, 8],
            [8, 4, 5, 6, 3, 4, 1, 5, 5, 1]
        ],
        [ -- 8
            [3, 4, 3, 2, 3, 3, 3, 2, 1, 1, 7, 1],
            [1, 1, 7, 1, 1, 1, 6, 5, 5, 5, 5],
            [5, 6, 5, 5, 5, 5, 4, 3, 2, 2, 3],
            [8, 4, 5, 1, 1, 1, 2, 3, 4, 5, 5, 1]
        ],
        [ --9
            [2, 3, 3, 2, 1, 1, 1, 7, 1],
            [7, 8, 8, 6, 4, 5, 5, 5, 5],
            [5, 5, 5, 4, 3, 2, 8, 3, 2, 3],
            [5, 3, 1, 4, 6, 5, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c2r
c2r = [ 
        [ -- duple 
            [ -- 0
                [Sb, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Sb],
                [Sb, MnD, Sm, Sb, MnD, Sm, Mn, Mn, MnD, Sm, Sb],
                [Sb, MnD, Sm, Mn, Sm, Sm, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, MnD, Sm, Sb, Sb, Mn, Mn, MnD, Sm, Sb]
            ],
            [ -- 1
                [Sb, Sm, Sm, Mn, MnD, Sm, Sm, Sm, SmD, Fs, Sb],
                [Sb, Sm, Sm, Sm, Sm, MnD, Sm, Sm, Sm, SmD, Fs, Sb],
                    -- correction: value 7 is Fs
                [Sb, Sm, Sm, Sm, Fs, Fs, MnD, Sm, Sm, Sm, SmD, Fs, Sb],
                [Sb, Sm, Sm, Mn, MnD, Sm, Sm, Sm, SmD, Fs, Sb]
            ],
            [ -- 2
                [Mn, Sm, Sm, SbD, Mn, MnD, Sm, Sb],
                [Mn, Sm, Sm, SbD, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, SbD, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, SbD, Mn, MnD, Sm, Sb]
            ],
            [ -- 3
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, MnD, Sm, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 4
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Sb]
            ],
            [ -- 5  
                [Sm, Sm, Sm, Sm, Mn, Mn, MnD, Sm, MnD, Sm, SbD, Mn, Sb],
                [Mn, Mn, Mn, Mn, Mn, Mn, Sb, MnD, Sm, Br],
                [Mn, Mn, Mn, MnD, Fs, Fs, Mn, Sm, Sm, Sm, Sm, MnD, Fs, Fs, MnD, Sm, Sb],
                [Sb, Mn, Mn, Sb, Mn, Sb, Mn, Mn, Mn, Sb]
            ],
            [ -- 6
                [MnD, Sm, Mn, Mn, Sb, Sb, Mn, Mn, Mn, Sb, Mn, Sb], -- unclear
                [MnD, Sm, Mn, Mn, Sb, MnD, Sm, Mn, Mn, Mn, Sm, Sm, MnD, Sm, Sb],
                [MnD, Sm, Mn, Mn, Sb, Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [MnD, Sm, Sm, Sm, Mn, Sb, Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb]
            ],
            [ -- 7
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, MnD, Sm, Sb],
                [Mn, Sm, Sm, SmD, Fs, Sm, Sm, MnD, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, MnD, Fs, Fs, Sm, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Mn, Sb]
            ],
            [ -- 8
                [Mn, Sm, Fs, Fs, Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Mn, Sm, Sm, Sm, Sm, SmD, Fs, Sb],
                [Mn, Sm, Sm, Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Mn, Sm, Sm, Sm, Fs, Fs, Sm, Sm, Sb]
            ],
            [ -- 9
                [Mn, Sm, Sm, Fs, Fs, Sm, SmD, Fs, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                    -- correction: value 1 looks like Sb
                [Mn, Sm, Sm, Fs, Fs, Fs, Fs, Sm, Sm, Sb],
                    -- correction: value 1 looks like Sb
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb]
                -- something is wrong XXX
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
            [7, 7, 7, 1, 2, 3, 4, 5, 5, 5],
            [5, 5, 5, 6, 8, 8, 7, 8],
            [3, 3, 3, 3, 8, 3, 2, 3],
            [3, 3, 3, 6, 6, 5, 5, 1]
        ],
        [ -- 1
            [2, 3, 2, 1, 7, 1, 2, 2, 2, 3, 2, 7, 1, 7, 1],
            [7, 7, 7, 5, 5, 4, 5, 4, 5, 5, 6, 5, 5, 5, 5],
            [5, 5, 4, 3, 2, 8, 7, 6, 7, 8, 4, 3, 2, 2, 3],
            [5, 3, 7, 1, 2, 3, 4, 5, 3, 2, 5, 3, 4, 5, 5, 1]
        ],
        [ --2
            [7, 2, 3, 4, 8, 8, 2, 4, 3, 4, 5, 5, 4, 5],
            [5, 7, 7, 6, 5, 4, 2, 5, 6, 7, 8, 5, 8, 7, 6, 7, 8],
            [2, 7, 1, 1, 7, 6, 7, 8, 2, 3, 3, 2, 8, 3, 2, 3],
            [5, 4, 3, 2, 1, 8, 6, 5, 1]
        ],
        [ --3
            [5, 6, 5, 3, 5, 5, 4, 3, 2, 3],
            [8, 8, 7, 8, 7, 6, 5, 5],
            [3, 4, 2, 8, 8, 8, 8, 8],
            [8, 4, 5, 6, 3, 4, 1, 1]
        ],
        [ --4
            [1, 1, 2, 1, 7, 3, 3, 3, 2, 2, 2, 3],
            [5, 6, 7, 5, 8, 8, 8, 8, 8, 7, 8],
            [3, 4, 4, 3, 5, 5, 5, 6, 5, 5, 5],
            [1, 4, 2, 3, 1, 1, 8, 4, 5, 5, 1]
        ],
        [ -- 5
            [7, 2, 8, 7, 8, 6, 8, 7, 6, 7, 8],
            [5, 5, 5, 5, 5, 4, 5, 5, 5],
            [3, 2, 3, 2, 3, 8, 3, 2, 2, 3],
            [3, 7, 1, 5, 3, 4, 1, 5, 5, 1]
        ],
        [ --6
            [5, 6, 5, 3, 5, 4, 3, 2, 8, 7, 8],
            [8, 8, 7, 8, 7, 6, 5, 5, 5, 5, 5],
            [3, 4, 2, 8, 8, 8, 7, 3, 2, 3],
            [8, 4, 5, 6, 3, 4, 1, 5, 1]
        ],
        [ -- 7
            [7, 7, 2, 1, 7, 3, 8, 4, 7, 1, 7, 1],
            [5, 5, 7, 5, 5, 5, 6, 6, 5, 5, 5],
            [2, 3, 5, 3, 2, 8, 8, 2, 2, 3, 2, 3],
            [5, 3, 7, 1, 2, 3, 4, 5, 3, 4, 5, 5, 1]
        ],
        [ -- 8
            [3, 2, 7, 8, 7, 6, 7, 8, 7, 8],
            [8, 6, 5, 4, 5, 4, 5, 5, 5, 5],
            [5, 4, 2, 8, 3, 8, 3, 2, 2, 3],
            [1, 4, 5, 6, 3, 4, 1, 5, 5, 1]
        ],
        [ --9
            [7, 1, 2, 2, 3, 1, 7, 1],
            [5, 6, 6, 5, 5, 4, 5, 5],
            [2, 6, 6, 7, 8, 8, 2, 3, 2, 3],
            [5, 4, 4, 4, 3, 6, 5, 1] -- CN: 5, *5, 4, 4, 3, 6, 5, 1
        ]
    ]
-- }}}2
-- {{{2 c3r
c3r = [ 
        [ -- duple 
            [ -- 0
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, MnD, Sm, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 1
                [Sb, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Sb, Sb, Sm, Mn, Sm, Mn, Mn, Sb, Sb, Mn, Mn, Sb],
                [Sb, Mn, Mn, Sb, Mn, MnD, Sm, Mn, Mn, Sb, Sb, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Mn, Mn, Sb, SbD, Mn, Sb]
                    -- correction: last value looks like Sm!
            ],
            [ -- 2
                [Sm, Mn, Sm, Sm, Mn, Sm, MnD, Sm, Sm, Sm, Mn, MnD, Sm, Br],
                [Sm, Mn, Sm, Mn, Mn, Mn, Mn, MnD, Fs, Fs, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Mn, Mn, Mn, MnD, Fs, Fs, Mn, Sm, Sm, Mn, MnD, Fs, Fs, Mn, Mn, Sb],
                [Sb, Mn, Mn, Sb, Mn, Sb, Mn, Sb, Sb]
            ],
            [ -- 3
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb, Sm, Sm, Sb],
                [Sb, Mn, Mn, MnD, Sm, Sb, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb, Sb]
            ],
            [ -- 4
                [Sb, Mn, Mn, Sm, Sm, Sm, Sm, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 5  
                [Sb, Mn, Mn, Mn, Mn, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 6
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, MnD, Sm, Mn, Mn, SbD, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn, Br, Sb]
            ],
            [ -- 7
                [Mn, Sm, Sm, Mn, MnD, Sm, Sm, Sm, Sm, Mn, Sm, Sb],
                [Mn, Sm, Sm, Mn, MnD, Sm, Sm, Sm, MnD, Sm, Sb],
                [Mn, Sm, Sm, Mn, MnD, Sm, Sm, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Sm, Fs, Fs, Fs, Fs, MnD, Sm, Mn, MnD, Sm, Sb]
            ],
            [ -- 8
                [Mn, Sm, Sm, SmD, Fs, Fs, Fs, Mn, Sm, Mn],
                [Mn, Sm, SmD, Fs, Sm, Sm, Sm, SmD, Fs, Mn],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, SmD, Fs, Mn],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Mn]
            ],
            [ -- 9
                [Sb, Mn, Mn, Mn, Mn, SbD, Mn, Sb],
                [Sb, Mn, Mn, Mn, Sb, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1

