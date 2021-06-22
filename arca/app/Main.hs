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

import System.Directory
    (removeFile)

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
        input       = readInput rawInput
        sections    = prepareInput input 
        lengths     = inputPhraseLengths sections
        metadata    = arkMetadata input

    perms <- inputPerms lengths

    let 
        score = makeMusicScore arca sections perms 
        mei   = score2mei arca metadata score

        tmpfileName = dropExtension outfileName ++ ".tmp"
        xmllint = unwords ["xmllint --format --noblanks --output", 
                            outfileName, tmpfileName]

    writeFile tmpfileName mei
    callCommand xmllint
    removeFile tmpfileName



-- Test contents of output before conversion to output format:
--  writeFile outfileName $ unlines [show input, show sections, show perms]



