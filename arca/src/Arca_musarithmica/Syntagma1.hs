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

import Arca_musarithmica.Syntagma1.Pinax1
    (s1p1)

import Arca_musarithmica.Syntagma1.Pinax2
    (s1p2)

import Arca_musarithmica.Syntagma1.Pinax3
    (s1p3)

import Arca_musarithmica.Syntagma1.Pinax4
    (s1p4)

-- | To build the /syntagma/, convert the /pinakes/ from lists to vectors.
-- 's1' has two /pinakes/: one for long penultimate syllables ('s1p1') and one
-- for short ('s1p2').
s1 = fromList [s1p1, s1p2, s1p3, s1p4] :: Syntagma

