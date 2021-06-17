{-|
Module      : Arca_musarithmica.Syntagma1.Pinax02
Description : Data for the ark, syntagma 1, pinax 2
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s1p2') is for phrases with short penultimate syllables. 
It contains five columns.
-}

module Arca_musarithmica.Syntagma1.Pinax02 (s1p2) where

import Data.Vector 
    (fromList)

import Aedifico
    (Dur (..),
     Pinax,
     Column (..),
     VpermTable,
     RpermTable,
     buildVpermTable,
     buildRpermTable)
 
-- * Pinax

-- | Pinax 2, for phrases with short penultimate syllables.
--
-- "Voces polysyllabae, quae penultimam Breuem habet"
s1p2 = fromList [c0, c1, c2, c3, c4] :: Pinax

-- ** Column 0 (2 syllables)
c0 = Column c0v c0r

-- *** vperms
c0v :: VpermTable
c0v = buildVpermTable
    [
        [ -- 0
            [2, 2],
            [8, 7],
            [5, 5],
            [8, 5]
        ],
        [ -- 1
            [2, 3],
            [7, 8],
            [5, 5],
            [5, 1]
        ],
        [ -- 2
            [3, 3],
            [8, 8],
            [5, 5],
            [8, 1]
        ],
        [ -- 3
            [6, 5],
            [1, 1],
            [4, 3],
            [4, 8]
        ],
        [ -- 4
            [2, 3],
            [5, 5],
            [7, 8],
            [5, 1]
        ],
        [ -- 5
            [3, 2],
            [8, 7],
            [5, 5],
            [8, 5]
        ],
        [ -- 6
            [5, 5],
            [8, 7],
            [3, 2],
            [8, 5]
        ],
        [ -- 7
            [2, 3],
            [7, 8],
            [5, 5],
            [5, 1]
        ],
        [ -- 8
            [6, 5],
            [8, 8],
            [4, 3],
            [4, 8]
        ],
        [ -- 9
            [0, 0],
            [0, 0],
            [0, 0],
            [0, 0] -- XXX missing, check source
        ]
    ]

-- *** rperms
c0r :: RpermTable
c0r = buildRpermTable 
    [
        [ -- duple
            [[Sb, Sb]],
            [[MnD, Sm]],
            [[SmD, Fs]],
            [[Sm, Sm]]
        ],
        [ -- triple major
            [[Br, Sb]],
            [[BrR, Sb, BrD]]
        ],
        [ -- triple minor
            [[Sb, Mn]],
            [[MnR, Mn, Mn]]
        ]
    ]

-- ** Column 1 (3 syllables)
c1 = Column c1v c1r

-- *** vperms
c1v :: VpermTable
c1v = buildVpermTable 
    [
        [ -- 0
            [5, 5, 5],
            [7, 7, 8],
            [2, 2, 3],
            [5, 5, 1]
        ],
        [ -- 1
            [4, 4, 5],
            [6, 6, 7],
            [2, 2, 3],
            [2, 2, 5]
        ],
        [ -- 2
            [3, 3, 4],
            [8, 8, 8],
            [5, 5, 6],
            [1, 1, 4]
        ],
        [ -- 3
            [2, 2, 3],
            [7, 7, 8],
            [5, 5, 5],
            [5, 5, 1]
        ],
        [ -- 4
            [4, 4, 3],
            [8, 8, 8],
            [6, 6, 5],
            [4, 4, 8]
        ], 
        [ -- 5
            [5, 5, 5],
            [7, 7, 8],
            [2, 2, 3],
            [5, 5, 1]
        ],
        [ -- 6
            [3, 3, 4],
            [8, 8, 8],
            [5, 5, 6],
            [8, 8, 4]
        ],
        [ -- 7 
            [4, 4, 2],
            [6, 6, 7],
            [2, 2, 2],
            [2, 2, 5]
        ],
        [ -- 8
            [2, 2, 3],
            [7, 7, 8],
            [5, 5, 5],
            [5, 5, 1]
        ],
        [ -- 9
            [6, 6, 5],
            [8, 8, 8],
            [4, 4, 3],
            [4, 4, 1]
        ]
    ]

-- *** rperms
c1r :: RpermTable
c1r = buildRpermTable 
    [
        [ -- duple
            [[SbD, Mn, Sb]],
            [[SmD, Mn, Sb]],
            [[SmD, Fs, Mn]],
            [[SmR, Mn, Sm, Sb]],
            [[MnR, Sb, Mn, Sb]],
            [[SmR, SmD, Fs, Sm]]
        ],
        [ -- triple major
            [[SbD, Mn, Sb]],
            [[Sb, Sb, Sb]],
            [[Br, Sb, BrD]]
        ],
        [ -- triple minor
            [[MnD, Sm, Mn]],
            [[Mn, Mn, Mn]],
            [[Sb, Mn, SbD]]
        ]
    ]


-- ** Column 2 (4 syllables)
c2 = Column c2v c2r

-- *** vperms
c2v :: VpermTable
c2v = buildVpermTable
    [
        [ -- 0
            [3, 2, 8, 7],
            [8, 7, 5, 5],
            [5, 5, 3, 2],
            [8, 5, 8, 5]
        ],
        [ -- 1
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [8, 4, 5, 1]
        ],
        [ -- 2
            [2, 3, 2, 2],
            [7, 8, 6, 7],
            [5, 5, 4, 5],
            [5, 8, 2, 5]
        ],
        [ -- 3
            [3, 4, 4, 3],
            [8, 8, 8, 8],
            [5, 6, 6, 5],
            [1, 4, 4, 1]
        ],
        [ -- 4
            [4, 3, 5, 5],
            [6, 6, 7, 8],
            [2, 8, 2, 3],
            [2, 6, 5, 1]
        ],
        [ -- 5
            [3, 4, 4, 3],
            [8, 8, 8, 8],
            [5, 6, 6, 5],
            [8, 4, 4, 1]
        ],
        [ -- 6: cf perm 4
            [4, 3, 5, 5],
            [6, 6, 7, 8],
            [2, 8, 2, 3],
            [2, 6, 5, 1]
        ],
        [ -- 7
            [4, 3, 5, 5],
            [6, 6, 7, 8],
            [2, 8, 2, 3],
            [2, 6, 5, 1]
        ],
        [ -- 8
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [1, 4, 5, 1]
        ],
        [ -- 9
            [3, 4, 4, 3],
            [8, 8, 8, 8],
            [5, 6, 6, 5],
            [1, 4, 4, 1]
        ]
    ]

-- *** rperms
c2r :: RpermTable
c2r = buildRpermTable
    [ 
        [ -- duple
            [[Sb, SbD, Mn, Sb]],
            [[Sb, MnD, Sm, Sb]],
            [[Mn, MnD, Sm, Mn]],
            [[Mn, SmD, Fs, Sb]],
            [[Mn, Sb, Mn, Sb]],
            [[SmR, Sm, SmD, Fs, Sb]]
        ],
        [ -- triple major
            [[Sb, Sb, Sb, BrD]],
            [[BrR, Sb, SbD, Mn, Sb]],
            [[BrR, Sm, Br, Sm, BrD]]
        ],
        [ -- triple minor
            [[Mn, Mn, Mn, SbD]],
            [[SbR, Mn, MnD, Sm, SbD]],
            [[SbR, Mn, Sb, Mn, SbD]]
        ]
    ]

-- ** Column 3 (5 syllables)
c3 = Column c3v c3r

-- *** vperms
c3v :: VpermTable
c3v = buildVpermTable
    [
      [ -- 0
        [7, 7, 8, 2, 3],
        [5, 5, 5, 7, 8],
        [2, 2, 3, 4, 5],
        [5, 5, 3, 2, 1]
      ],
      [ -- 1
        [3, 3, 4, 3, 4],
        [8, 8, 8, 7, 8],
        [5, 5, 6, 7, 6],
        [8, 8, 6, 5, 4]
      ],
      [ -- 2
        [4, 4, 3, 2, 3],
        [8, 2, 8, 7, 8],
        [6, 6, 5, 5, 5],
        [4, 2, 3, 5, 1]
      ],
      [ -- 3
        [7, 8, 2, 2, 2],
        [5, 5, 6, 6, 7],
        [2, 3, 4, 4, 5],
        [5, 3, 2, 2, 5]
      ],
      [ -- 4
        [8, 2, 7, 7, 8],
        [6, 6, 5, 5, 5],
        [3, 4, 2, 2, 3],
        [6, 4, 5, 5, 1]
      ],
      [ -- 5
        [7, 8, 7, 7, 8],
        [5, 5, 5, 5, 5],
        [2, 3, 2, 2, 3],
        [5, 1, 5, 5, 1]
      ],
      [ -- 6
        [3, 2, 7, 7, 8],
        [7, 6, 5, 5, 5],
        [5, 4, 2, 2, 3],
        [3, 4, 5, 5, 1]
      ],
      [ -- 7
        [5, 6, 5, 5, 5],
        [8, 8, 8, 8, 8],
        [3, 4, 2, 2, 3],
        [1, 4, 5, 5, 1]
      ],
      [ -- 8
        [8, 2, 7, 7, 8],
        [6, 6, 5, 5, 5],
        [3, 4, 2, 2, 3],
        [6, 4, 5, 5, 1]
      ],
      [ -- 9
        [6, 5, 4, 4, 3],
        [8, 8, 8, 8, 8],
        [4, 5, 6, 6, 8],
        [4, 1, 4, 4, 1]
      ]
    ]

-- *** rperms
c3r :: RpermTable
c3r = buildRpermTable
    [
      [ -- duple
        [[SbD, Mn, SbD, Mn, Sb]],
        [[Mn, Mn, MnD, Sm, Sb]],
        [[Sm, Sm, SmD, Fs, Sb]],
        [[MnR, Sb, Mn, SbD, Mn, Sb]],
        [[SmR, Mn, Sm, SmD, Fs, Mn]],
        [[MnD, Sm, MnD, Sm, Sb]]
      ],
      [ -- triple major
        [[Br, Sb, Br, Sb, BrD]],
        [[Br, Sb, SbD, Mn, Sb]],
        [[SbR, Sb, Sb, SbD, Mn, Sb]]
      ],
      [ -- triple minor
        [[Sb, Mn, Sb, Mn, SbD]],
        [[Sb, Mn, MnD, Sm, Mn]],
        [[MnR, Mn, Mn, MnD, Sm, Mn]]
      ]
    ]

-- ** Column 4 (6 syllables)
c4 = Column c4v c4r

-- *** vperms
c4v :: VpermTable
c4v = buildVpermTable
    [
      [ -- 0
        [2, 3, 3,  2, 2, 3],
        [7, 8, 8,  7, 7, 8],
        [5, 5, 5,  5, 5, 5],
        [5, 3, 1,  5, 5, 1]
      ],
      [ -- 1
        [5, 6, 5,  5, 5, 5],
        [8, 8, 8,  7, 7, 8],
        [3, 4, 3,  2, 2, 3],
        [8, 4, 8,  5, 5, 1]
      ],
      [ -- 2
        [2, 3, 2,  2, 2, 2],
        [7, 8, 7,  6, 6, 7],
        [5, 5, 5,  4, 4, 5],
        [5, 1, 5,  2, 2, 5]
      ],
      [ -- 3
        [5, 4, 3,  2, 2, 3],
        [3, 2, 1,  1, 7, 1],
        [8, 6, 5,  6, 5, 5],
        [1, 2, 3,  4, 5, 1]
      ],
      [ -- 4 
        [6, 6, 5,  6, 6, 5],
        [8, 8, 8,  8, 8, 8],
        [4, 4, 3,  4, 4, 3],
        [4, 4, 8,  4, 4, 8]
      ],
      [ -- 5 
        [2, 3, 3,  2, 2, 3],
        [7, 8, 8,  7, 7, 8],
        [5, 5, 5,  5, 5, 5],
        [5, 3, 1,  5, 5, 1]
      ],
      [ -- 6 
        [5, 4, 3,  2, 2, 3],
        [8, 8, 8,  7, 7, 8],
        [3, 6, 5,  5, 5, 5],
        [1, 4, 1,  5, 5, 1]
      ],
      [ -- 7 
        [2, 2, 2,  2, 2, 2],
        [7, 6, 7,  6, 6, 7],
        [5, 4, 5,  4, 4, 5],
        [5, 2, 5,  2, 2, 5]
      ],
      [ -- 8 
        [8, 4, 5,  6, 6, 5],
        [6, 8, 8,  8, 8, 8],
        [4, 6, 3,  4, 4, 3],
        [4, 4, 1,  4, 4, 1]
      ],
      [ -- 9
        [5, 4, 3,  2, 2, 3],
        [7, 6, 8,  8, 7, 8],
        [3, 2, 5,  6, 5, 5],
        [5, 2, 3,  4, 5, 1]
      ]
    ]

-- *** rperms
c4r :: RpermTable
c4r = buildRpermTable
    [
      [ -- duple
        [[Sb, Mn, Mn, MnD, Sm, Sb]],
        [[Sb, Mn, Mn, SbD, Mn, Sb]],
        [[Mn, Sm, Sm, SmD, Fs, Mn]],
        [[SmR, Sm, Sm, Sm, SmD, Fs, Mn]],
        [[MnR, Mn, Mn, Mn, MnD, Sm, Sb]],
        [[Sm, Fs, Fs, SmD, Fs, Sb]],
        [[Mn, Sb, Mn, MnD, Sm, Sb]]
      ],
      [ -- triple major
        [[Sb, Sb, Sb, SbD, Mn, Sb]],
        [[Sb, Sb, Sb, Br, Sb, BrD]],
        [[BrR, Sb, Br, Sb, Br, Sb, BrD]]
      ],
      [ -- triple minor
        [[Mn, Mn, Mn, MnD, Sm, Mn]],
        [[Mn, Mn, Mn, Sb, Mn, SbD]]
      ]
    ]
