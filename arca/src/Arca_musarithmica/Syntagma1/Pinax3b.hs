{-|
Module      : Arca_musarithmica.Syntagma1.Pinax 3b
Description : Data for the ark, syntagma 1, pinax 3b (Dactylus)
Copyright   : Data from Kircher, 1650; implementation (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Sub-structures used by @Arca_musarithmica@ to build Kircher's ark.
This /pinax/ ('s1p3b') is for poetry in Dactylic meter ('__'__).
It contains four columns.
-}

module Arca_musarithmica.Syntagma1.Pinax3a (s1p3b) where

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

s1p3b = fromList []
