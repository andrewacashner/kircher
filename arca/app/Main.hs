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
import Cogito
import Scribo

-- * Main
main :: IO()
main = do

    -- Test pulling data from the ark.
    let 
        music = getMusic arca Simple Short 4 TripleMinor Soprano 1
        ly = map pitch2ly music

    putStrLn(show music)
    putStrLn(show ly)

