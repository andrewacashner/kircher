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
import Fortuna

-- * Main
main :: IO ()
main = do
    
    -- Test pulling data from the ark.
    vperms <- randomList (0, 9)
    rperms <- randomList (0, 4)
    let 
        choices = take 5 (zip vperms rperms)
        music = map (\ x -> getChorus arca Simple Long (fst x) Duple (snd x)) choices 
    
    putStrLn $ show $ music

    -- TODO error: vector index out of bounds
    
    -- TODO pivot/glue

   -- manually : compose arca Simple Long 5 Duple 1
