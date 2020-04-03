{- |
Fortuna: (Theologically) dangerous chance operations!
-}

module Fortuna where

import System.Random

-- | A @Perm@ stores the random number choices used to select voice and rhythm
-- permutations.
data Perm = Perm {
    voice  :: Int,
    rhythm :: Int
}

-- | Make a pair of random numbers to select vperm and rperm:
--   - vperm one of 10 vperms per column
--   - rperm based on how many rperms there are per meter (how to know?)
choosePerms :: IO Perm
choosePerms = do
    let 
        vpermBounds = (0, 9)
        rpermBounds = (0, 3)
    v <- getStdRandom (randomR vpermBounds)
    r <- getStdRandom (randomR rpermBounds)

    let p = Perm { voice = v, rhythm = r }
    return p

