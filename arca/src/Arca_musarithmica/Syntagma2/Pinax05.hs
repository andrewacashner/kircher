{-|
Module      : Arca_musarithmica.Syntagma2.Pinax05
Description : Data for the ark, syntagma 2, pinax 5 (Enneasyllabic & Decasyllabic)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s2p5') contains four columns for successive strophes. Each
contains pairs of four-voice vperms and four-voice rperms.

Kircher intends this for meters with eleven and ten syllable lines (p. 116),
with the paradigm being /Amant venena parricida/.

> "PINAX V. Melothesiae [-as?] Floridae & artificiosae Musarithmos continens. Pro metris Enneasyllabis & Decasyllabis."

This pinax is for /toni/ (modes) 1, 2, 3, 4, 9, and 10.
-}

module Arca_musarithmica.Syntagma2.Pinax05 (s2p5) where

import Aedifico
    (
        Dur (..),
        Column (..),
        buildVpermTable,
        buildRpermTable,
        buildPinax
    )

-- | Pinax 5
s2p5 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = Column c0v c0r

-- {{{2 c0v
c0v = buildVpermTable 
    [ 
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
c0r = buildRpermTable 
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
-- {{{1 c1
c1 = Column c1v c1r

-- {{{2 c1v
c1v = buildVpermTable 
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
-- {{{2 c1r
c1r = buildRpermTable 
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

