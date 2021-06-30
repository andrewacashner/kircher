{-|
Module      : Arca_musarithmica.Syntagma2.Pinax03
Description : Data for the ark, syntagma 2, pinax 3
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p3') contains four columns for successive strophes. Each
contains pairs of four-voice vperms and four-voice rperms.
Kircher intends this for "Anacreontic meters" (p. 109), with the paradigm being /O ter quaterque felix/.

> PINAX III. "Musarithmos Melothesias Floridae siue Artificiosae continèns.
> Pro metris Anacreonticis."

This pinax is for /toni/ (modes) 1, 2, 3, 4, 9, and 10.
-}

module Arca_musarithmica.Syntagma2.Pinax03 (s2p3) where

import Aedifico
    (
        Dur (..),
        Column (..),
        buildVpermTable,
        buildRpermTable,
        buildPinax
    )

-- | Pinax 2
s2p3 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = Column c0v c0r

-- {{{2 c0v
c0v = buildVpermTable 
    [ 
        [ -- 0
            [8, 7, 8, 2, 3, 1, 7],
            [5, 5, 6, 7, 7, 6, 5],
            [3, 2, 3, 4, 5, 4, 2],
            [8, 5, 8, 7, 3, 4, 5] -- value 4 obscured
        ],
        [ -- 1
            [8, 2, 2, 3, 4, 5, 5],
            [5, 4, 5, 1, 1, 1, 1, 7],
            [3, 2, 2, 8, 8, 3, 2],
            [8, 8, 7, 7, 6, 5, 5]
        ],
        [ --2
            [3, 3, 2, 5, 4, 3, 2],
            [1, 1, 7, 7, 6, 5, 5], -- value 1 obscured
            [5, 4, 4, 3, 1, 7, 1, 7],
            [8, 6, 7, 3, 4, 5, 5]
        ],
        [ --3
            [2, 3, 3, 3, 4, 5, 4, 3, 4, 5],
            [7, 7, 1, 1, 8, 1, 7],
            [5, 5, 5, 5, 4, 3, 1, 2],
            [5, 3, 1, 8, 7, 6, 5]
        ],
        [ --4
            [2, 2, 2, 3, 2, 2, 4, 4, 3, 2, 1, 2],
            [7, 7, 7, 1, 7, 7, 2, 1, 7, 6, 7, 6, 7],
            [5, 5, 5, 5, 5, 4, 3, 2, 4, 4],
            [5, 5, 5, 1, 5, 7, 6, 5, 4, 7]
        ],
        [ -- 5
            [7, 8, 8, 7, 3, 2, 7, 1, 7, 1],
            [5, 5, 4, 5, 6, 5, 5],
            [2, 1, 2, 1, 4, 4, 3, 2, 3],
            [5, 6, 6, 5, 3, 4, 5, 1]
        ],
        [ --6
            [5, 5, 3, 6, 5, 4, 3, 4, 5],
            [1, 1, 1, 1, 7, 1, 7],
            [3, 3, 5, 4, 2, 8, 2],
            [8, 8, 8, 4, 5, 6, 5]
        ],
        [ -- 7
            [2, 3, 4, 5, 4, 4, 3, 2, 3],
            [7, 1, 7, 6, 5, 6, 5, 5],
            [5, 5, 4, 3, 2, 1, 7, 1, 7, 1],
            [5, 1, 2, 3, 4, 5, 1]
        ],
        [ -- 8
            [8, 4, 3, 6, 5, 4, 3, 2, 3],
            [6, 5, 4, 8, 6, 6, 5, 5],
            [8, 8, 8, 8, 8, 8, 8],
            [4, 1, 1, 4, 3, 4, 1]
        ],
        [ --9
            [8, 4, 3, 2, 1, 7, 1],
            [6, 7, 7, 1, 7, 5, 5],
            [8, 2, 5, 4, 3, 2, 3],
            [4, 5, 5, 6, 7, 3, 4, 5, 1]
        ]
    ]
-- }}}2
-- {{{2 c0r
c0r = buildRpermTable 
    [ 
        [ -- duple 
            [ -- 0
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn],
                [Sb, Mn, Mn, Mn, Mn, Mn, Mn]
            ],
            [ -- 2
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Mn, Sb]
            ],
            [ -- 3
                [Mn, Sm, Sm, Sm, Sm, MnD, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sb, Mn, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sb, Sb]
            ],
            [ -- 4
                [Sb, Fs, Fs, MnD, Sm, Mn, Mn, Sm, Fs, Fs, Mn, Mn],
                [Sm, Fs, Fs, MnD, Sm, Mn, Sm, Fs, Fs, Sm, Mn, Sm, Mn],
                [Sm, Fs, Fs, MnD, Sm, Sm, Sm, Mn, Sb, Mn],
                [Sm, Fs, Fs, MnD, Sm, MnD, Fs, Fs, Sb, Mn]
            ],
            [ -- 5
                [Sb, Mn, Mn, Mn, Sb, Mn, Mn, Sb, Mn, Sb],
                [Sb, Mn, Mn, Sb, Sb, Br, Sb],
                [Sb, Sb, Mn, Sb, Mn, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Br, Sb]
            ],
            [ -- 6
                [Mn, Sm, Sm, MnD, Mn, Mn, Fs, Fs, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sb, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sb, Sb],
                [Mn, Sm, Sm, MnD, Sm, Sb, Sb]
            ],
            [ -- 7
                [MnR, Mn, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Fs, Fs, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, Sm, Sm, Fs, Fs, Mn, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sb, Sb]
            ],
            [ -- 8
                [Mn, Sb, Mn, Mn, Sb, Sb, Sm, Sm, Sb],
                [Sb, Sb, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Sb, Sb, Mn, Mn, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Br]
            ],
            [ -- 9
                [Mn, Sb, Sb, Sb, Sb, Mn, Sb],
                [Sb, Mn, Mn, Mn, Mn, Br, Sb],
                [Mn, Sb, Mn, Sb, Sb, Sb, Sb],
                [Sb, Mn, Mn, Mn, Mn, MnD, Sm, Sb, Sb]
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
            [7, 8, 2, 3, 2, 1, 7, 1, 7, 1],
            [5, 5, 6, 7, 7, 6, 5, 5],
            [2, 3, 4, 5, 4, 4, 3, 2, 3],
            [5, 8, 7, 3, 4, 5, 1]
        ],
        [ -- 1
            [8, 2, 2, 3, 2, 3, 2, 8, 2, 3],
            [5, 6, 7, 7, 7, 7, 5, 6, 7, 7],
            [3, 4, 5, 4, 3, 4, 5],
            [8, 7, 6, 5, 4, 3, 4, 5, 6, 7, 8, 7, 3]
        ],
        [ --2
            [2, 3, 2, 2, 1, 1, 7, 1],
            [7, 1, 7, 7, 5, 5, 5],
            [5, 5, 4, 5, 3, 2, 3],
            [5, 1, 2, 3, 4, 5, 1]
        ],
        [ --3
            [7, 8, 2, 3, 5, 4, 3, 2, 3],
            [5, 5, 6, 7, 6, 5, 5],
            [2, 3, 4, 5, 3, 1, 2, 7, 1, 7, 1],
            [5, 1, 7, 3, 4, 5, 1]
        ],
        [ --4
            [5, 5, 5, 6, 5, 5, 4, 5],
            [1, 1, 1, 1, 7, 1, 7],
            [3, 3, 3, 4, 2, 8, 2],
            [8, 8, 8, 4, 5, 6, 5]
        ],
        [ -- 5
            [8, 2, 3, 3, 5, 4, 5],
            [5, 6, 7, 7, 1, 7, 7, 7],
            [3, 4, 2, 5, 3, 3, 2, 1, 2, 3],
            [8, 7, 5, 6, 3, 7, 3]
        ],
        [ --6
            [2, 3, 2, 2, 1, 1, 7, 1],
            [7, 1, 7, 7, 5, 5, 5],
            [5, 5, 4, 3, 3, 2, 3],
            [5, 1, 2, 3, 4, 5, 1]
        ],
        [ -- 7
            [8, 2, 8, 2, 2, 2, 2],
            [5, 7, 1, 7, 7, 6, 7],
            [3, 4, 4, 4, 5, 4, 5],
            [8, 7, 6, 7, 5, 2, 5]
        ],
        [ -- 8
            [5, 7, 6, 6, 5, 4, 5],
            [8, 2, 8, 2, 3, 3, 2, 3],
            [3, 2, 4, 4, 5, 6, 7, 7],
            [8, 5, 6, 7, 7, 7, 3] -- value 2 obscured
        ],
        [ --9
            [3, 2, 5, 5, 4, 2, 3, 2, 1, 2, 3],
            [1, 7, 7, 1, 7, 7, 7],
            [5, 5, 5, 3, 6, 6, 5, 4, 5],
            [1, 5, 5, 6, 7, 7, 3]
        ]
    ]

-- }}}2
-- {{{2 c1r
c1r = buildRpermTable 
    [ 
        [ -- duple 
            [ -- 0
                [Sb, MnD, Sm, MnD, Fs, Fs, Sm, Mn, Sm, Sb],
                [Sb, Mn, Sm, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sm, Sm, Mn, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 1
                [MnR, Mn, Mn, Mn, Sb, Mn, MnD, Fs, Fs, Mn, Sb],
                [MnD, Sm, Mn, Mn, Sb, Mn, Sm, Sm, Sm, Sb, Sb], -- unclear
                [Sb, Sb, Sb, Mn, Mn, Sb, Sb],
                [Sb, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 2
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Sm, SmD, Fs, Mn, Sb]
            ],
            [ -- 3
                [Sb, Mn, Mn, Sm, Mn, Mn, Sm, Mn, Sb],
                [Sb, MnD, Fs, Sb, Mn, Sb, Sb],
                [Sb, Mn, Mn, Sm, Sm, Sm, Sm, Sm, Mn, Sm, Sb],
                [Sb, Mn, Mn, Mn, Mn, Sb, Sb]
            ],
            [ -- 4
                [Mn, Sm, Sm, Mn, Mn, Mn, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 5
                [MnR, Mn, MnD, Sm, Mn, Mn, Sb, Sb],
                [MnD, Sm, Mn, Mn, Mn, Mn, Sb, Sb],
                [Sb, Sm, Sm, Mn, Mn, MnD, Fs, Fs, Mn, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 6
                [SmR, Sm, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [SmR, Sm, Sm, Sm, Sm, Sm, Mn, Sb],
                [SmR, Sm, Sm, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Sm, SmD, Fs, Mn, Sb]
            ],
            [ -- 7
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb],
                [Sb, MnD, Sm, Mn, Sb, Mn, Sb],
                [Sb, MnD, Sm, Mn, Mn, Sb, Sb]
            ],
            [ -- 8
                [Mn, Sm, Sm, Sm, Sm, Mn, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb],
                [Mn, Sm, Sm, Sm, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Sm, Sm, Mn, Sb]
            ],
            [ -- 9
                [Mn, Sm, Sm, Sm, Sm, Sm, Mn, Fs, Fs, Mn, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb],
                [Mn, Sm, Sm, Sm, Sm, Sm, Sm, Sb, Sb],
                [Mn, Sm, Sm, Mn, Mn, Sb, Sb]
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
c2r = buildRpermTable 
    [ 
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
-- {{{1 c3
c3 = Column c3v c3r

-- {{{2 c3v
c3v = buildVpermTable 
    [ 
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
-- {{{2 c3r
c3r = buildRpermTable 
    [ 
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

