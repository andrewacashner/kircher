{-|
Module      : Arca_musarithmica.Syntagma2.Pinax06
Description : Data for the ark, syntagma 2, pinax 6 (Sapphic, Hendecasyllabic)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p6') contains /three/ columns for successive strophes
(actually meaning verse lines). Each contains pairs of four-voice vperms and
four-voice rperms.

Kircher intends this for Saphhic meters with 11-syllable lines (p. 119), with
the paradigm being /Ut queant laxis resonare fibris/.

> "PINAX VI. Musarithmos Melothesias Floridae & artificiosae continens. Pro
metris sapphicis quibuslibet Hendecasyllabis."

This pinax is for /toni/ (modes) 1, 2, 3, 4, 9, and 10 in all three "strophes."
-}

module Arca_musarithmica.Syntagma2.Pinax06 (s2p6) where

import Aedifico
    ( Dur (..) 
    , buildPinax
    )

-- | Pinax 5
s2p6 = buildPinax [c0, c1, c2]

-- {{{1 c0
c0 = (c0v, c0r)

-- {{{2 c0v
c0v = [ 
        [ -- 0
            [3, 2, 3, 5, 4, 3, 4, 5, 5, 4, 3, 4, 2, 3, 2, 3],
            [8, 7, 8, 8, 7, 5, 8, 7, 6, 5, 6, 7, 1, 7, 7],
            [5, 5, 3, 3, 2, 8, 2, 3, 8, 2, 5, 3, 6, 5, 4, 5],
            [8, 5, 8, 6, 5, 3, 4, 5, 6, 7, 3]
        ],
        [ -- 1
            [5, 5, 4, 3, 2, 3, 3, 3, 2, 3, 4, 4],
            [7, 7, 2, 1, 7, 8, 8, 6, 7, 6, 5, 6, 7],
            [5, 5, 5, 5, 5, 5, 5, 1, 7, 1, 2],
            [3, 3, 7, 1, 5, 3, 3, 4, 5, 4, 1]
        ],
        [ --2
            [8, 7, 8, 8, 7, 3, 5, 4, 3, 6, 5, 4, 5],
            [5, 5, 5, 4, 5, 5, 5, 6, 5, 8, 2, 3, 2, 3],
            [3, 2, 3, 2, 8, 2, 8, 8, 7, 3, 4, 5, 6, 7, 7],
            [1, 5, 8, 6, 5, 3, 4, 5, 6, 7, 3]
        ],
        [ --3
            [3, 3, 3, 4, 5, 4, 3, 2, 2, 1, 7, 6, 7, 1],
            [5, 5, 5, 6, 5, 1, 7, 6, 5, 5, 4, 5, 5],
            [8, 8, 8, 8, 8, 7, 7, 8, 2, 3, 2, 1, 2, 3],
            [1, 4, 3, 4, 1, 5, 5, 6, 6, 5, 1]
        ],
        [ --4
            [4, 4, 5, 3, 2, 5, 5, 4, 2, 2, 2, 8, 2, 3, 3, 2, 2, 8, 2, 3, 4, 7, 5, 5, 4, 3, 2, 1, 7, 1, 7],
            [7, 7, 5, 8, 7, 3, 3, 7, 7, 7, 7, 5, 7, 7, 7, 7, 7, 5, 5, 8, 7, 6, 6, 5, 7, 7, 1, 7, 1, 5, 5, 4, 3, 4, 5],
            [4, 4, 5, 3, 4, 5, 5, 4, 5, 3, 2, 8, 3, 3, 2, 3, 5, 5, 4, 3, 2, 8, 2],
            [7, 7, 5, 8, 7, 3, 3, 7, 5, 8, 7, 6, 5, 4, 4, 3, 3, 3, 2, 1, 5, 1, 5, 6, 5]
        ],
        [ -- 5
            [8, 8, 8, 8, 7, 4, 4, 7, 7, 7, 7],
            [5, 5, 5, 5, 5, 6, 6, 5, 5, 4, 5],
            [3, 3, 3, 3, 2, 3, 2, 8, 2, 2, 3, 3, 3, 2, 8, 2, 3],
            [1, 1, 1, 1, 5, 4, 4, 3, 4, 5, 6, 7, 3]
        ],
        [ --6
            [7, 6, 7, 8, 7, 8, 8, 2, 3, 3, 2, 3],
            [5, 6, 5, 5, 5, 5, 5, 6, 5, 6, 7, 7],
            [2, 2, 2, 3, 2, 3, 3, 8, 3, 4, 5, 4, 5],
            [5, 4, 5, 1, 5, 3, 3, 4, 1, 7, 3]
        ],
        [ -- 7
            [2, 3, 2, 1, 7, 1, 7, 1, 2, 3, 2, 5, 4, 3, 4, 3, 2, 3],
            [7, 7, 7, 5, 5, 4, 5, 5, 5, 7, 6, 7, 6, 5, 6, 5, 5],
            [5, 5, 4, 3, 2, 8, 3, 3, 3, 4, 5, 4, 3, 4, 3, 8, 2, 8, 7, 8, 7, 8],
            [5, 3, 7, 1, 5, 6, 3, 3, 3, 2, 1, 7, 3, 4, 5, 1]
        ],
        [ -- 8
            [3, 3, 3, 2, 1, 2, 2, 5, 8, 4, 2, 7, 3, 2, 1, 1, 5, 1, 7, 1],
            [5, 5, 1, 7, 1, 7, 6, 7, 1, 6, 7, 5, 5, 6, 5, 5, 5],
            [8, 8, 8, 5, 6, 5, 3, 4, 2, 2, 8, 4, 4, 3, 2, 3],
            [5, 1, 4, 7, 5, 1, 4, 5, 5, 1]
        ],
        [ --9
            [7, 7, 7, 1, 2, 3, 3, 2, 7, 1, 7, 1, 7, 1],
            [5, 5, 5, 5, 4, 5, 8, 4, 5, 5, 4, 3, 4, 5, 5],
            [2, 2, 2, 1, 7, 7, 6, 5, 6, 6, 7, 3, 3, 2, 8, 2, 8, 2, 3],
            [5, 5, 5, 3, 2, 1, 1, 7, 3, 6, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r = [ 
        [ -- duple 
            [ -- 0
                [Sb, Mn, Sm, Mn, Fs, Fs, Mn, Sb, MnD, Mn, Mn, Sm, Sm, Mn, Sm, Mn],
                [Sb, Mn, Mn, Sb, Mn, Sm, SmD, Fs, Fs, Fs, Mn, Mn, Mn, Sb, Sb],
                [SbD, Sm, Sm, Mn, Sm, Sm, Mn, Sb, Sm, Mn, Sm, Sm, Mn, Sm, Mn, Sb],
                [Sb, Mn, Mn, Sb, Mn, Sb, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [Mn, SmD, Fs, Mn, Mn, SmD, Fs, Sm, Fs, Fs, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, SmD, Fs, Sm, Mn, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, SmD, Fs, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, SmD, Fs, Sm, Sm, Sb, Sb]
            ],
            [ -- 2
                [Sb, Mn, Mn, Sb, Mn, Mn, Mn, MnD, Mn, Sm, Mn, Sb],
                [Sb, Mn, Sb, Mn, Mn, MnD, Sm, Mn, Mn, Mn, Sm, Mn, Sm, Sb],
                [Sb, Mn, Sb, Sm, Sm, Mn, Mn, Sb, Mn, Sm, Mn, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Sb, Mn, Sb, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 3
                [Mn, Sm, Sm, Mn, MnD, Mn, Sm, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Mn, Sm, Fs, Fs, Mn, Mn, MnD, Sm, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, SmD, Fs, Sm, Fs, Fs, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sm, Sm, Sm, Sm, Sb, Sb]
            ],
            [ -- 4
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Mn, MnD, Sm, Mn, Mn, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Sb, Sb, Mn, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Sb, Sb],
                [MnR, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Mn, MnD, Sm, Mn, Mn, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Sb, Mn, Mn, Mn, Mn, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [LgR, MnR, Mn, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Sb, MnR, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [LgR, MnR, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 5
                [Mn, Sm, Sm, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Mn, Sm, Mn, Fs, Fs, Sm, Sm, Mn, Mn, Sm, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, MnD, Sm, Sm, Sm, Sm, Sm, Sb, Sb] -- unclear
            ],
            [ -- 6
                [SmR, Sm, Sm, Sm, Mn, Sm, SmR, SmR, Fs, Fs, Sm, Sm, Sm, Sm, Mn],
                [SmR, Sm, Sm, Sm, Mn, Sm, Fs, Fs, Sm, Fs, Fs, Sb, Mn],
                [SmR, Sm, Sm, Sm, Mn, Sm, Fs, Fs, Sm, Sm, Sm, Sm, Mn, Mn],
                [SmR, Sm, Sm, Sm, Mn, Sm, Fs, Fs, Sm, Sm, Sb, Mn]
            ],
            [ -- 7
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Mn, Mn, Sb, Mn, MnD, Fs, Fs, MnD, Sm, Mn, Sb],
                [MnD, Sm, Mn, Mn, MnD, Sm, Mn, MnD, Sm, Sb, Mn, MnD, Fs, Fs, Sb, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Mn, Mn, MnD, Fs, Fs, Sm, Mn, Sm, Sb],
                [MnD, Sm, Mn, Mn, Mn, Mn, Mn, MnD, Sm, Mn, Sb, Mn, Mn, Sb, Sb, Sb]
            ],
            [ -- 8
                [MnR, SmR, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Mn, Sm, Mn, Mn, MnD, Fs, Fs, Sm, Sm, Sb, Mn, Sb],
                [MnR, SmR, Sm, Sm, Sm, Sm, Mn, Fs, Fs, Mn, Mn, Mn, Mn, MnD, Sm, Mn, Sb, Sb, Sb],
                [MnR, SmR, Sm, Sm, Sm, Sm, Sm, Sb, Mn, MnD, Mn, Sm, MnD, Sm, Mn, Mn, Sb, Sb],
                [BrR, MnR, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb, Sb]
            ],
            [ -- 9
                [Mn, Sm, Sm, Mn, Mn, MnD, Sm, Mn, Mn, Sb, Mn, Sb, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sm, Fs, Fs, Sm, Sm, MnD, Sm, Mn, Sm, Sm, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Br, Sb]
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
        ],
        [ -- 1
        ],
        [ --2
        ],
        [ --3
        ],
        [ --4
        ],
        [ -- 5
        ],
        [ --6
        ],
        [ -- 7
        ],
        [ -- 8
        ],
        [ --9
        ]
    ]
-- }}}2
-- {{{2 c1r
c1r = [ 
        [ -- duple 
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
    ]
-- }}}2
-- }}}1
-- {{{1 c2
c2 = (c2v, c2r)

-- {{{2 c2v
c2v = [ 
        [ -- 0
        ],
        [ -- 1
        ],
        [ --2
        ],
        [ --3
        ],
        [ --4
        ],
        [ -- 5
        ],
        [ --6
        ],
        [ -- 7
        ],
        [ -- 8
        ],
        [ --9
        ]
    ]
-- }}}2
-- {{{2 c2r
c2r = [ 
        [ -- duple 
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
    ]
-- }}}2
-- }}}1

