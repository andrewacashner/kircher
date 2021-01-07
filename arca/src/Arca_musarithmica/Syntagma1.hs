{-|
Module      : Arca_musarithmica.Syntagma0
Description : Data for Syntagma 1
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

"Syntagma I. Melothesias siue Contrapuncti simplicis."

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
-}

module Arca_musarithmica.Syntagma1 (s1) where

import Data.Vector 
    (fromList)

import Aedifico 
    (Syntagma)

import Arca_musarithmica.Syntagma1.Pinax01
    (s1p1)

import Arca_musarithmica.Syntagma1.Pinax02
    (s1p2)

import Arca_musarithmica.Syntagma1.Pinax03
    (s1p3)

import Arca_musarithmica.Syntagma1.Pinax04
    (s1p4)

import Arca_musarithmica.Syntagma1.Pinax05
    (s1p5)

import Arca_musarithmica.Syntagma1.Pinax06
    (s1p6)

import Arca_musarithmica.Syntagma1.Pinax07
    (s1p7)

import Arca_musarithmica.Syntagma1.Pinax08
    (s1p8)

import Arca_musarithmica.Syntagma1.Pinax09
    (s1p9)

import Arca_musarithmica.Syntagma1.Pinax10
    (s1p10)

import Arca_musarithmica.Syntagma1.Pinax11
    (s1p11)

-- | To build the /syntagma/, convert the /pinakes/ from lists to vectors.
s1 :: Syntagma
s1 = fromList [s1p1, s1p2, s1p3, s1p4, s1p5, s1p6, 
               s1p7, s1p8, s1p9, s1p10, s1p11]

