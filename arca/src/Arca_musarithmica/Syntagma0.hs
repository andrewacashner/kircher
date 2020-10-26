{-|
Module      : Arca_musarithmica.Syntagma0
Description : Data for Syntagma 0
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

"Syntagma I. Melothesias siue Contrapuncti simplicis."

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
We replace Kircher's 1-indexed numbering with 0-indexed numbering, hence
's0p0' for the first /pinax/.
-}

module Arca_musarithmica.Syntagma0 (s0) where

import Data.Vector 
    (fromList)

import Aedifico 
    (Syntagma)

import Arca_musarithmica.Syntagma0.Pinax0 
    (s0p0)

import Arca_musarithmica.Syntagma0.Pinax1 
    (s0p1)

-- | To build the /syntagma/, convert the /pinakes/ from lists to vectors.
-- 's0' has two /pinakes/: one for long penultimate syllables ('s0p0') and one
-- for short ('s0p1').
s0 = fromList [s0p0, s0p1] :: Syntagma

