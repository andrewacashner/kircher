{-|
Module      : Arca_musarithmica.Syntagma1.Pinax3a
Description : Data for the ark, syntagma 1, pinax 3a (Adonius)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s1p3a') is for poetry in Adonic meter ('__'_).
It contains four columns.
-}

module Arca_musarithmica.Syntagma1.Pinax3a (s1p3a) where

import Data.Vector 
    (fromList)

import Aedifico
    (Dur (..),
     Pinax,
     Column,
     VpermTable,
     RpermTable,
     buildVpermTable,
     buildRpermTable)

s1p3a = fromList [c0, c1, c2, c3] :: Pinax

c0 = (c0v, c0r) :: Column

c0v :: VpermTable
c0v = buildVpermTable
    [
        [ -- 0
            [5, 5, 6, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 1
            [3, 2, 4, 5, 5],
            [8, 2, 8, 2, 3],
            [5, 5, 6, 7, 8],
            [8, 7, 6, 5, 1]
        ],
        [ -- 2
            [5, 5, 5, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 3, 2, 3],
            [8, 8, 8, 5, 1]
        ],
        [ -- 3
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 8, 5, 1] -- XXX unclear
        ],
        [ -- 4
            [6, 5, 5, 6, 5],
            [8, 8, 8, 8, 8],
            [8, 5, 5, 4, 3],
            [4, 3, 3, 4, 1]
        ],
        [ -- 5
            [3, 3, 2, 3, 3],
            [8, 8, 7, 8, 8],
            [5, 5, 5, 5, 5],
            [8, 8, 5, 1, 1]
        ],
        [ -- 6
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 1, 5, 1]
        ],
        [  -- 7
            [5, 5, 6, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 4, 2, 3],
            [8, 8, 4, 5, 1]
        ],
        [ -- 8
            [4, 4, 3, 2, 3],
            [8, 8, 8, 7, 8],
            [6, 6, 5, 5, 5],
            [4, 4, 8, 5, 1]
        ],
        [ -- 9
            [5, 5, 5, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 3, 3, 2, 3],
            [8, 8, 8, 5, 1]
        ]
    ]

