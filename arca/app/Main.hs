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
import Scribo

-- * Main
main :: IO()
main = do

    -- Test pulling data from the ark.
    putStrLn(show (getMusic arca Simple Short 5 TripleMinor Tenor 1))

    -- Test displaying a pitch
    putStrLn(show music)
        where
            music = map pitch2ly 
                [Pitch PCc 4 Mn Sh, 
                 Pitch PCd 5 SmD Na,
                 Pitch PCe 2 BrD Fl]
