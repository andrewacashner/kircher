{-|
Module      : Arca_musarithmica.Syntagma2.Pinax02
Description : Data for the ark, syntagma 2, pinax 2
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
    (
        Dur (..),
        Column (..),
        buildVpermTable,
        buildRpermTable,
        buildPinax
    )

-- | Pinax 2
s2p2 = buildPinax [c0, c1, c2, c3]

-- {{{1 c0
c0 = Column c0v c0r

-- {{{2 c0v
c0v = buildVpermTable 
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

