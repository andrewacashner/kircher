{- | 
 - /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
 - Automatically compose music using Kircher's /Arca musarithmica/ from
 - /Musurgia universalis/ (Rome, 1650).
 -
 -  Currently just testing.
 -}

module Main where

import Arca 
import Arca.Base 

-- * Main
main :: IO()
main = do

    -- Test pulling data from the ark.
    putStrLn(show (getMusic arca Simple Short 5 TripleMinor Tenor 1))
