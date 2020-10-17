{-|
Module      : Fortuna
Description : (Theologically) dangerous chance operations: generate random indices
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

Kircher's ark does require a small amount of free choice on the part of the
user, in selecting which voice permutation and rhythm permutation to use from
each column.  We take the (for Kircher) theologically dangerous route of
chance operations and generate a random number for the index of the voice and
rhythm permutations.
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
--
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

