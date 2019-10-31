-- | Build Kircher's Arca
module Modules.Arca.Base where
import Data.Vector (fromList)
import Modules.Arca (Arca, Syntagma)
import Modules.Arca.Syntagma0.Pinax0 (s0p0)
import Modules.Arca.Syntagma0.Pinax1 (s0p1)

arca = fromList [s0] :: Arca

-- * Syntagma 0
-- “Syntagma I. Melothesias siue Contrapuncti simplicis.”
s0 = fromList [s0p0, s0p1] :: Syntagma

