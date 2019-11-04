-- | Build Kircher's Arca
module Arca.Base where

import Data.Vector (fromList)
import Arca (Arca, Syntagma)
import Arca.Syntagma0.Pinax0 (s0p0)
import Arca.Syntagma0.Pinax1 (s0p1)

arca = fromList [s0] :: Arca

-- * Syntagma 0
-- “Syntagma I. Melothesias siue Contrapuncti simplicis.”
s0 = fromList [s0p0, s0p1] :: Syntagma

