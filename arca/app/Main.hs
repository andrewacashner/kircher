{- | 
 - /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
 - Automatically compose music using Kircher's /Arca musarithmica/ from
 - /Musurgia universalis/ (Rome, 1650).
 -
 - Implemented in Haskell by Andrew Cashner, <andrew.cashner@rochester.edu>,
 - 2021
 -}

module Main where

import System.Environment
    (getArgs)

import System.Process
    (callCommand)

import System.FilePath
    ( dropExtension
    , takeDirectory
    )

import System.Directory
    (removeFile)

import Arca_musarithmica 
    (arca)

import Aedifico 

import Lectio
    ( arkMetadata
    , readInput
    , prepareInput
    , inputPhraseLengths
    )

import Fortuna
    (inputPerms)

import Cogito

import Scribo.MEI

-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Output to PDF via Lilypond.
main :: IO ()
main = do
    
    [infileName, outfileName] <- getArgs
    rawInput <- if infileName == "-"
                    then do 
                        source <- getContents
                        return(source)
                    else do 
                        source <- readFile infileName
                        return(source)

    let 
        outfile | infileName == "-" && outfileName == "-" 
                    = "musica.mei"
                | outfileName == "-"
                    = dropExtension infileName ++ ".mei"
                | otherwise 
                    = outfileName

        input       = readInput rawInput
        sections    = prepareInput input 
        lengths     = inputPhraseLengths sections
        metadata    = arkMetadata input

    perms <- inputPerms lengths

    let 
        score = makeMusicScore arca sections perms 
        mei   = score2mei arca metadata score

    writeFile outfile mei

--        tmpfile = dropExtension outfile ++ ".tmp"
--        xmllint = unwords ["xmllint --format --noblanks --output", 
--                            outfile, tmpfile]

--    writeFile tmpfile mei
--    callCommand xmllint
--    removeFile tmpfile



-- Test contents of output before conversion to output format:
--  writeFile outfileName $ unlines [show input, show sections, show perms]



