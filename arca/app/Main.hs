{- | 
 - /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
 - Automatically compose music using Kircher's /Arca musarithmica/ from
 - /Musurgia universalis/ (Rome, 1650).
 -
 - Implemented in Haskell by Andrew Cashner, <andrew.cashner@rochester.edu>,
 - 2020
 -}

module Main where

import System.Environment
    (getArgs)

import System.Process
    (callCommand)

import System.FilePath
    (dropExtension,
     takeDirectory)

import Arca_musarithmica 
    (arca)

import Lectio
    (arkMetadata,
     readInput,
     prepareInput,
     inputPhraseLengths)

import Fortuna
    (inputPerms)

import Scribo
    (compose)


-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Output to PDF via Lilypond.
main :: IO ()
main = do
    
    [infileName, outfileName] <- getArgs
    rawInput <- readFile infileName

    let 
        input     = readInput rawInput
        sections  = prepareInput input 
        lengths   = inputPhraseLengths sections
        metadata  = arkMetadata input

    perms <- inputPerms lengths

    let 
        music = compose arca metadata sections perms 

        ly_outfile = (dropExtension outfileName) ++ ".ly"
        lycommand = unwords ["lilypond -I ~/lib/ly -o", 
                             takeDirectory outfileName,
                             ly_outfile]

 
    writeFile ly_outfile music
    callCommand lycommand

--    writeFile outfileName $ unlines [show input, show sections, show perms]
