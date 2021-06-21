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

import Aedifico 

import Lectio
    (arkMetadata,
     readInput,
     prepareInput,
     inputPhraseLengths)

import Fortuna
    (inputPerms)

import Cogito
--    (makeMusicScore)

import Scribo
    (compose)

import Scribo.MEI

-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Output to PDF via Lilypond.
main :: IO ()
main = do
    
    [infileName, outfileName] <- getArgs
    rawInput <- readFile infileName

    let 
        mei_outfile = (dropExtension outfileName) ++ ".mei"
        input       = readInput rawInput
        sections    = prepareInput input 
        lengths     = inputPhraseLengths sections
        metadata    = arkMetadata input

    perms <- inputPerms lengths

    let 
        score = makeMusicScore arca sections perms 
        mei   = score2mei metadata score

    writeFile mei_outfile mei

-- Output with old Scribo (to Lilypond)
--  let
--        music = compose arca metadata sections perms 

--        lycommand = unwords ["lilypond -I ~/lib/ly -o", 
--                             takeDirectory outfileName,
--                             ly_outfile]
--  writeFile ly_outfile music
--  callCommand lycommand

-- Test contents of output before conversion to output format:
--  writeFile outfileName $ unlines [show input, show sections, show perms]

-- Test basic MEI writing of Notes
--    let 
--        note0 = Note (newRest Mn) blankSyllable
--        note1 = Note (Pitch PCc 4 Sb Na) (Syllable "lau" First)
--        note2 = Note (Pitch PCc 4 SbD Sh) (Syllable "da" Middle)
--        note3 = Note (Pitch PCd 4 Mn Na) (Syllable "te" Last)
--        notes = [note0, note1, note2, note3]
--    
--    putStrLn $ notes2mei notes


