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
        phrase = head text -- Just test with first phrase of text for now

        music  = compose arca Simple Duple perm phrase 
    
    putStrLn music
   
--  To test prepareText:
--    putStr $ showSentence text
    
-- TODO pivot/glue multiple music phrases together

