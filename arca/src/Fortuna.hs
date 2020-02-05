module Fortuna where

import System.Random
import Data.List

randomList :: (Int, Int) -> IO [Int]
randomList interval = 
    newStdGen >>= return . unfoldr (Just . randomR interval)

