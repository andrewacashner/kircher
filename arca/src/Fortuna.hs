{- |
Fortuna: Dangerous chance operations!
-}

module Fortuna where

import System.Random

-- | Make a pair of random numbers to select vperm and rperm:
--   - vperm based on syllable count: minimum 2, maximum 6 (always?)
--   - rperm based on how many rperms there are per meter (how to know?)
choosePerms :: IO (Int, Int)
choosePerms = do
    let 
        vpermBounds = (2, 6)
        rpermBounds = (0, 3)
    vperm <- getStdRandom (randomR vpermBounds)
    rperm <- getStdRandom (randomR rpermBounds)
    return (vperm, rperm)

