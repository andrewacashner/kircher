{-|
Module      : Arca_musarithmica.Syntagma2
Description : Data for Syntagma 2
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

"Syntagma II. Musarithmos Melothesias Floridae & Artificiosae continens"

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
-}

module Arca_musarithmica.Syntagma2 (s2) where

import Data.Vector 
    (fromList)

import Aedifico 
    (Syntagma)

import Arca_musarithmica.Syntagma2.Pinax01
    (s2p1)

import Arca_musarithmica.Syntagma2.Pinax02
    (s2p2)
 
-- import Arca_musarithmica.Syntagma2.Pinax03
--     (s2p3)
-- 
-- import Arca_musarithmica.Syntagma2.Pinax04
--     (s2p4)
-- 
-- import Arca_musarithmica.Syntagma2.Pinax05
--     (s2p5)

-- | To build the /syntagma/, convert the /pinakes/ from lists to vectors.
s2 :: Syntagma
s2 = fromList [s2p1, s2p2]
-- , s2p3, s2p4, s2p5]

