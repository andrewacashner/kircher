{-|
Module      : Arca_musarithmica.Syntagma2.Pinax02
Description : Data for the ark, syntagma 2, pinax 2 (Iambic, Euripidaeic)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p2') contains four columns for successive strophes. Each
contains pairs of four-voice vperms and four-voice rperms.
Kircher intends this for "Iambic, Euripidaeic, and Hectasyllabic meters" (p. 106), with the paradigm being /Ave Maris stella/.

> PINAX II. "Musarithmi Melothesias Floridae siue Artificiose pro metris Iambicis, Euripedaeis, Hectasyllabis"

He does not give any limitations of modes.
-}

module Arca_musarithmica.Syntagma2.Pinax02 (s2p2) where

import Aedifico
    ( Dur (..)
    , buildPinax
    )

-- | Pinax 2
s2p2 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = (c0v, c0r)

-- {{{2 c0v
c0v =  
    [ 
        [ -- 0
            [2, 4, 3, 2, 1, 5, 1, 7, 6, 7],
            [7, 6, 5, 4, 3, 2, 8, 2],
            [2, 2, 7, 7, 5, 5],
            [5, 2, 3, 7, 1, 5]
        ],
        [ -- 1
            [7, 3, 3, 2, 7, 1, 7, 1],
            [5, 5, 6, 6, 5, 5],
            [2, 8, 8, 4, 4, 3, 2, 3],
            [5, 3, 4, 4, 5, 1]
        ],
        [ --2
            [5, 7, 7, 2, 8, 7, 6, 7],
            [3, 4, 5, 7, 5, 5],
            [8, 2, 3, 4, 3, 2, 1, 2],
            [1, 7, 3, 7, 1, 5]
        ],
        [ --3
            [8, 8, 8, 2, 2, 1, 7, 1, 7],
            [5, 5, 5, 5, 5, 5],
            [3, 3, 3, 2, 3, 2],
            [8, 8, 8, 7, 8, 5]
        ],
        [ --4
            [5, 4, 3, 2, 3, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1],
            [5, 6, 7, 1, 5, 4, 5, 6, 7, 6, 5, 3],
            [8, 2, 3, 4, 8, 7, 8, 2, 8, 7, 8, 8, 7, 8],
            [8, 7, 6, 6, 6, 5, 4, 5, 5, 1]
        ],
        [ -- 5
            [6, 6, 7, 1, 1, 7, 1],
            [4, 4, 4, 5, 5, 5],
            [8, 8, 2, 3, 2, 3],
            [4, 4, 2, 1, 5, 1]
        ],
        [ --6
            [4, 3, 2, 1, 7, 1, 7, 6, 7, 1],
            [8, 7, 6, 5, 6, 5, 5, 5],
            [5, 4, 3, 2, 8, 4, 3, 2, 3],
            [5, 4, 3, 2, 1, 4, 4, 5, 1, 5, 1]
        ],
        [ -- 7
            [7, 1, 2, 3, 2, 3],
            [5, 5, 7, 1, 1, 7, 1],
            [2, 3, 4, 5, 5, 5],
            [5, 3, 2, 1, 5, 1]
        ],
        [ -- 8
            [3, 2, 1, 7, 1, 7, 6, 7, 1],
            [7, 6, 5, 4, 5, 5],
            [5, 4, 2, 1, 2, 3, 2, 1, 2, 3],
            [3, 4, 5, 6, 5, 1]
        ],
        [ --9
            [4, 4, 3, 3, 2, 3],
            [1, 1, 1, 1, 1, 7, 6, 7, 1],
            [6, 6, 5, 5, 5, 5],
            [4, 4, 1, 1, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r =  
    [ 
        [ -- duple 
            [ -- 0
                [SbD, Mn, Mn, Mn, Sm, Sm, Sb, Sm, Sm, Sb],
                [SbD, Mn, Mn, Mn, SbD, Sm, Sm, Sb], -- or notes 2-3 from end are Mn
                [SbD, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Sb, Br]
            ],
            [ -- 1
                [SbD, Mn, Mn, Mn, Mn, Sb, Mn, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb]
            ],
            [ -- 2
                [SbD, Mn, Mn, Mn, MnD, Fs, Fs, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, MnD, Fs, Fs, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 3
                [SbD, Mn, Sb, Sb, Mn, Sm, Sm, Sb, Sb],
                [SbD, Mn, Sb, Sb, Br, Sb],
                [SbD, Mn, Sb, Sb, Br, Sb], -- unclear
                [SbD, Mn, Sb, Sb, Br, Sb]
            ],
            [ -- 4
                [SbD, Mn, Mn, Mn, Sb, Mn, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Sb, Sb],
                [MnR, Mn, MnD, Sm, Mn, Mn, MnD, Sm, Sb, Sb, Sb, Br, Sb],
                [BrR, MnR, Mn, MnD, Sm, Mn, Mn, Sb, Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb], 
                    -- first rest unclear: I added the MnR after 
                [BrR, SbD, Mn, Mn, Mn, Mn, Mn, Sb, Sb, Sb, Sb]
            ],
            [ -- 5
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [SbR, MnR, Sm, Sm, Sm, Sm, Sm, Mn, Fs, Fs, Mn, Sb],
                [SbR, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Sb],
                [MnR, Sm, Sm, Sm, Sm, Mn, MnD, Sm, Sb, Sb],
                [Sm, Sm, Sm, Sm, Mn, Mn, Mn, Sm, Sm, Sb, Sb]
            ],
            [ -- 7
                [MnD, Sm, Mn, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb] -- unclear
            ],
            [ -- 8
                [SbD, Sm, Sm, Mn, MnD, Fs, Fs, Mn, Sb],
                [SbD, Mn, MnD, Sm, Sb, Sb],
                [SbD, Mn, Mn, Sm, Sm, Sm, Fs, Fs, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 9
                [SbD, Mn, SbD, Mn, Sb, Sb],
                [SbD, Mn, SbD, Mn, Sm, Fs, Fs, Mn, Sb],
                [SbD, Mn, SbD, Mn, Sb, Sb],
                [SbD, Mn, SbD, Mn, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c1
c1 = (c1v, c1r)

-- {{{2 c1v
c1v =  
    [ 
        [ -- 0
            [3, 2, 1, 7, 6, 5, 5],
            [1, 7, 6, 5, 4, 1, 4, 3, 2, 3],
            [5, 5, 3, 3, 8, 8],
            [1, 5, 6, 3, 4, 1]
        ],
        [ -- 1
            [8, 2, 3, 3, 4, 7],
            [5, 5, 1, 7, 6, 5],
            [3, 2, 8, 3, 3, 2, 3],
            [8, 7, 6, 5, 4, 3]
        ],
        [ --2
            [5, 5, 5, 4, 3, 2, 8, 2],
            [7, 7, 2, 2, 2, 1, 7, 1, 7],
            [2, 5, 5, 5, 5, 5],
            [5, 5, 7, 7, 8, 5]
        ],
        [ --3
            [8, 2, 3, 5, 4, 5],
            [5, 5, 8, 7, 7, 7],
            [3, 2, 8, 3, 3, 2, 8, 2, 3],
            [8, 7, 6, 3, 7, 3]
        ],
        [ --4
            [8, 2, 3, 3, 2, 2, 2, 2],
            [5, 1, 6, 7, 6, 7],
            [3, 4, 5, 5, 4, 5, 5, 4, 5],
            [8, 8, 8, 7, 2, 5]
        ],
        [ -- 5
            [2, 3, 2, 8, 2, 8, 8, 7, 6, 7],
            [7, 1, 5, 6, 4, 5, 4, 5],
            [5, 4, 4, 3, 2, 2],
            [5, 6, 7, 8, 2, 2, 5]
        ],
        [ --6
            [3, 3, 4, 5, 6, 5],
            [5, 6, 7, 8, 7, 8, 8, 8, 8],
            [3, 2, 8, 2, 3, 4, 5, 6, 6, 5, 5, 5, 4, 3, 4, 3],
            [8, 7, 6, 5, 4, 3, 4, 1]
        ],
        [ -- 7
            [8, 8, 8, 7, 6, 7],
            [5, 5, 6, 5, 5, 4, 5],
            [3, 3, 2, 2, 2, 2],
            [8, 8, 4, 5, 2, 5]
        ],
        [ -- 8
            [8, 3, 2, 8, 4, 3, 2, 5, 4, 3, 3, 4, 3, 3, 2, 1, 2, 3],
            [7, 1, 7, 6, 1, 7, 7, 1, 7, 6, 5],
            [3, 4, 3, 2, 5, 4, 3, 2, 1, 2, 3, 1, 2, 3, 4, 5],
            [5, 6, 5, 4, 7, 6, 5, 8, 7, 6, 6, 6, 5, 4, 3]
        ],
        [ --9
            [3, 2, 2, 2, 2, 2],
            [1, 7, 6, 7, 6, 7],
            [5, 5, 4, 5, 5, 4, 5],
            [1, 2, 3, 4, 5, 6, 7, 8, 2, 2, 7, 8, 2, 5]
        ]
    ]

-- }}}2
-- {{{2 c1r
c1r =  
    [ 
        [ -- duple 
            [ -- 0
                [SbD, Mn, Mn, Mn, Sb, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sm, Sm, Sb, Sm, Sm, Sb],
                [SbD, Mn, Mn, Mn, Sb, Br],
                [SbD, Mn, Mn, Mn, Sb, Br]
            ],
            [ -- 1
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [SbD, Mn, Mn, Mn, SbD, Sm, Sm, Sb],
                [SbD, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb]
            ],
            [ -- 3
                [Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Sm, Fs, Fs, Mn, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 4
                [MnD, Sm, Mn, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 5
                [Sb, MnD, Fs, Fs, Mn, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, MnD, Sm, Mn, Sb, Mn, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb, Sb]
            ],
            [ -- 6
                [SbD, Mn, Sb, Sb, Br, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb, Br, Sb],
                [MnD, Sm, Sm, Sm, Sm, Fs, Fs, Mn, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Sb],
                [MnD, Sm, Mn, Mn, Sb, Sb, Br, Sb]
            ],
            [ -- 7
                [MnD, Sm, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Mn, Sb],
                [MnD, Sm, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 8
                [Mn, MnD, Sm, Mn, MnD, Sm, Mn, MnD, Sm, Sb, Mn, Mn, Mn, Mn, Sm, Sm, Sb, Sb],
                [MnR, Mn, MnD, Sm, Sm, Sm, Sb, Mn, Br, Sb, Br, Sb], -- unclear
                [SbR, Mn, MnD, Sm, Mn, MnD, Sm, MnD, Sm, Sm, Sm, Sm, Sm, Mn, Mn, Br, Sb],
                [MnR, Mn, MnD, Sm, Mn, MnD, Sm, Mn, MnD, Sm, Mn, Mn, Mn, Mn, Br, Sb]
            ],
            [ -- 9
                [Sb, Sb, Mn, Mn, Sb, Sb], -- unclear
                [Sb, Sb, Mn, Mn, Sb, Sb],
                [Sb, Sb, Mn, Mn, Mn, Mn, Sb], 
                    -- unclear; last note obscured?
                [Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sb, Sb]
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
            [2, 5, 4, 4, 3, 2, 2],
            [7, 1, 2, 1, 1, 7, 6, 7],
            [5, 4, 3, 2, 5, 6, 4, 2, 5, 4, 5],
            [5, 6, 7, 8, 2, 5]
        ],
        [ -- 1
            [5, 4, 3, 4, 5, 5, 4, 5],
            [7, 1, 7, 1, 2, 1, 7],
            [5, 6, 5, 4, 5, 2, 3, 2, 8, 2],
            [3, 2, 8, 7, 6, 5]
        ],
        [ --2
            [1, 2, 3, 4, 5, 5, 4, 5],
            [5, 4, 5, 6, 7, 6, 7],
            [3, 2, 8, 2, 2, 2],
            [8, 7, 6, 5, 2, 5]
        ],
        [ --3
            [5, 5, 6, 5, 5, 4, 5],
            [8, 8, 8, 7, 8, 7],
            [3, 3, 4, 3, 2, 3, 2, 8, 2],
            [8, 8, 4, 5, 6, 6]
        ],
        [ --4
            [7, 2, 1, 1, 1, 7, 6, 7],
            [5, 7, 5, 6, 4, 5, 5, 4, 5],
            [3, 4, 3, 3, 2, 2],
            [3, 7, 1, 1, 2, 5]
        ],
        [ -- 5
            [3, 3, 3, 3, 3, 2, 3],
            [7, 7, 1, 7, 7, 7],
            [5, 5, 3, 5, 4, 5],
            [3, 4, 5, 5, 6, 5, 7, 3]
        ],
        [ --6
            [3, 4, 6, 5, 5, 4, 5], -- last 3 notes obscured XXX
            [7, 7, 1, 2, 1, 7],
            [5, 4, 3, 2, 3, 2, 8, 2],
            [3, 2, 8, 7, 6, 5]
        ],
        [ -- 7
            [5, 5, 3, 5, 4, 5],
            [1, 7, 1, 7, 7, 7],
            [3, 2, 8, 3, 3, 2, 3],
            [8, 5, 6, 3, 7, 3]
        ],
        [ -- 8
            [3, 3, 2, 1, 7, 1, 7, 1],
            [8, 6, 5, 4, 5, 5],
            [5, 5, 4, 3, 2, 8, 2, 3],
            [8, 4, 5, 6, 5, 1]
        ],
        [ --9
            [5, 5, 1, 2, 3, 1, 2, 5, 4, 3, 1, 2, 1, 7, 1],
            [8, 8, 5, 6, 7, 5, 6, 5, 6, 7, 1, 6, 5, 5],
            [5, 5, 8, 2, 3, 8, 4, 3, 2, 3],
            [8, 8, 5, 6, 7, 5, 6, 4, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c2r
c2r =  
    [ 
        [ -- duple 
            [ -- 0
                [Mn, Sb, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                -- [MnD, Sm, Mn, Sm, Mn, Sm, Sm, Sm, Sb, Mn, Sb],
                -- something is incorrect here; possible correction below
                [MnD, Sm, MnD, Sm, Mn, Mn, Sm, Sm, Sb, Mn, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb]
            ],
            [ -- 1
                [Mn, Mn, Sm, Sm, MnD, Sm, Mn, Sb],
                [Sm, Mn, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Fs, Fs, MnD, Sm, Mn, Sm, Sm, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb] -- correction? fourth note is Sb
            ],
            [ -- 2
                [Sb, MnD, Sm, Mn, Mn, Mn, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Sb, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 3
                [MnD, Sm, Mn, Mn, Mn, Mn, Sb],
                [MnD, Sm, Mn, Mn, Sb, Sb],
                [MnD, Sm, Sm, Sm, Sm, Fs, Fs, Sb, Sb],
                [MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 4
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Br, Sb]
            ],
            [ -- 5
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Mn, Sm, Sm, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 7
                [MnD, Sm, Mn, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Mn, Mn, Mn, Mn, Mn, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 8
                [MnD, Sm, Mn, Mn, Mn, Sb, Mn, Sb],
                [Sb, Sb, Mn, Mn, Sb, Sb],
                [Mn, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, Sb, Mn, Mn, Sb, Sb]
            ],
            [ -- 9
                [BrR, SbD, Mn, Mn, Mn, Mn, Mn, Sb, Mn, Mn, Sb, Sb, Mn, Sb, Mn, Sb],
                -- correction? values 9-10 look like Sm
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb, MnD, Sm, Br, Sb, Br, Sb, Sb],
                [LgR, BrR, SbD, Mn, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                -- correction: fourth value is Sb
                [LgR, SbD, Mn, Mn, Mn, Mn, Mn, Br, Sb, Sb, Sb]
            ]
        ]
    ]
-- }}}2
-- }}}1
-- {{{1 c3
c3 = (c3v, c3r)

-- {{{2 c3v
c3v =  
    [ 
        [ -- 0
            [3, 2, 8, 2, 7, 1, 7, 1],
            [1, 7, 5, 6, 5, 5],
            [5, 4, 3, 8, 4, 3, 2, 3],
            [1, 2, 3, 4, 5, 1]
        ],
        [ -- 1
            [5, 4, 4, 3, 2, 3, 4, 3, 4, 5, 5],
            [1, 7, 7, 6, 5, 6, 7, 1, 7, 1],
            [5, 4, 3, 2, 1, 7, 1, 2, 1, 2, 3],
            [8, 7, 6, 5, 4, 3, 4, 5, 1, 5, 6, 5, 1]
        ],
        [ --2
            [5, 5, 3, 1, 2, 1, 5, 5, 3, 1, 3, 2, 1],
            [8, 8, 7, 5, 5, 6, 7, 1, 7, 1],
            [5, 5, 3, 8, 2, 8, 2, 3, 4, 5, 3],
            [8, 8, 7, 5, 6, 5, 8] -- unclear
        ],
        [ --3
            [7, 1, 2, 3, 4, 1, 4, 3, 2, 2, 1, 7, 1, 7, 1],
            [5, 7, 6, 5, 4, 5, 5],
            [2, 7, 1, 1, 7, 6, 5, 6, 7, 6, 2, 1, 7, 3, 2, 3],
            [5, 4, 1, 2, 3, 4, 5, 1]
        ],
        [ --4
            [8, 8, 2, 3, 2, 3],
            [6, 5, 4, 2, 5, 5, 5],
            [1, 1, 1, 7, 1, 1, 7, 1],
                -- correction: value 1 is "2" (starting with ii7?)
            [4, 3, 2, 1, 5, 1]
        ],
        [ -- 5
            [1, 1, 7, 1, 1, 7, 1],
            [6, 6, 5, 4, 5, 5],
            [4, 4, 2, 8, 2, 3],
            [4, 4, 5, 6, 5, 1]
        ],
        [ --6
            [4, 3, 2, 1, 7, 5, 1, 7, 1],
            [6, 6, 6, 6, 5, 5],
            [8, 4, 3, 2, 8, 2, 3, 2, 2],
            [4, 4, 5, 6, 5, 1]
        ],
        [ -- 7
            [4, 4, 4, 3, 2, 3],
            [6, 6, 6, 5, 5, 5],
            [1, 2, 1, 7, 1, 1, 7, 1],
            [4, 4, 5, 1, 5, 1]
        ],
        [ -- 8
            [1, 2, 3, 5, 4, 4, 3, 2, 3],
            [5, 5, 8, 7, 6, 5, 5],
            [3, 2, 8, 3, 8, 2, 7, 1, 7, 1],
            [8, 7, 6, 3, 4, 5, 1]
        ],
        [ --9
            [5, 5, 6, 5, 4, 1, 4, 3, 2, 3],
            [1, 1, 1, 7, 6, 5, 4, 5],
            [3, 3, 4, 8, 8, 8],
            [8, 8, 4, 3, 4, 1]
        ]
    ]
-- }}}2
-- {{{2 c3r
c3r =  
    [ 
        [ -- duple 
            [ -- 0
                [SbD, Mn, MnD, Sm, Sm, Mn, Sm, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Sm, Mn, Sm, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [BrR, SbR, SbD, Mn, Mn, Mn, Sm, Sm, Sm, Fs, Fs, Sb, Sb],
                [BrR, SbD, Mn, Mn, Mn, MnD, Sm, Mn, Sb, Mn, Sb],
                [SbR, SbD, Mn, Mn, Mn, Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Sb, MnD, Sm, Mn, Mn, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [MnD, Sm, Mn, Mn, Sb, Sb, MnD, Sm, Mn, Mn, Mn, Mn, Sb],
                [SbR, MnD, Sm, Mn, Mn, MnD, Sm, Sb, SbD, Mn, Sb],
                [BrR, MnD, Sm, Mn, Mn, Sb, Sm, Sm, Sm, Sm, Sb, Sb],
                [BrR, SbR, MnD, Sm, Mn, Mn, Sb, Sb, Sb]
            ],
            [ -- 3
                [Sm, Sm, Sm, Sm, Sm, Sm, Mn, Sb, Mn, MnD, Fs, Fs, Sb, Mn, Sb],
                [Mn, Mn, Sb, SbD, Mn, Br, Sb],
                [Mn, Mn, Sb, Sm, Fs, Fs, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Sb],
                    -- correction?: value 2 looks like Sm
                [Sb, Sb, Sb, SbD, Sm, Sm, Sb, Sb]
                    -- correction: value 5 is Mn
            ],
            [ -- 4
                [Mn, Mn, Mn, Mn, Sb, Sb],
                [Mn, Mn, Sm, Sm, Mn, Sb, Sb],
                [Mn, Mn, Sm, Sm, Mn, Mn, Mn, Sb],
                [Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 5  
                [SbD, Mn, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [Mn, Mn, Mn, Mn, Sm, Sm, Sb, Mn, Sb],
                [MnD, Sm, Mn, Mn, Br, Sb],
                [Sb, Mn, Mn, Mn, Sm, Sm, Mn, Mn, Sb],
                [Sb, Sb, Mn, Mn, Sb, Sb]
            ],
            [ -- 7
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb],
                [SbD, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 8
                [SbD, Mn, Mn, Sb, Mn, Mn, Mn, Sb, Sb],
                [SbD, Mn, Mn, Mn, Sb, Br, Sb],
                [SbD, Mn, Mn, Mn, MnD, Sm, Mn, Sb, Mn, Sb],
                [SbD, Mn, Mn, Mn, Sb, Br, Sb]
            ],
            [ -- 9
                [SbD, Mn, Mn, Mn, Sm, Sm, Sb, Sm, Sm, Sb],
                [SbD, Mn, MnD, Sm, SbD, Sm, Sm, Sb],
                    -- correction: value 5, dot added
                [SbD, Mn, Mn, Mn, Br, Sb],
                [SbD, Mn, Mn, Mn, Sb, Br]
            ]
        ]
    ]
-- }}}2
-- }}}1

