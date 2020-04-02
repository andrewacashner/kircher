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

    perms <- choosePerms
    input <- getLine

    let 
        vperm = fst perms
        rperm = snd perms
        text = prepareText input

        -- Just test with first phrase of text for now
        phrase1 = head text
        syl = phraseSylCount phrase1
        len = phrasePenultLength phrase1

        music = compose arca vperm rperm Simple Duple syl len
    
    putStrLn music
   
--  To test prepareText:
--    putStr $ showSentence text
    
-- TODO pivot/glue multiple music phrases together

