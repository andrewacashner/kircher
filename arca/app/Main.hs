{- | 
 - /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
 - Automatically compose music using Kircher's /Arca musarithmica/ from
 - /Musurgia universalis/ (Rome, 1650).
 -
 -  Currently just testing.
 -}

module Main where

import Arca_musarithmica (arca)
import Aedifico
import Lectio
import Cogito
import Scribo
import Fortuna

-- * Main
main :: IO ()
main = do

    perm  <- choosePerms
    input <- getLine

    let 
        text   = prepareText input
        -- music = compose arca Simple Duple perm $ head text
        music = unwords $ map (\ p -> compose arca Simple Duple perm p) $ phrases text
    
    putStrLn music

-- testing with just the first phrase of text
-- TODO for now, each phrase produces a separate \score
-- need to pivot/glue multiple music phrases together
-- need to select different perms for each phrase

