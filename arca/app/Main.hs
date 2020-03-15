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

--    perms <- choosePerms
--
--    let 
--        vperm = fst perms
--        rperm = snd perms
--        music = compose arca Simple Long vperm Duple rperm
--    
--    putStrLn music
--    
--    -- TODO pivot/glue
--
    interact cleanup
