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

import System.Exit
    (exitFailure)

import System.FilePath
    (dropExtension)

import Arca_musarithmica 
    (arca)

import Lectio
    ( arkMetadata
    , readInput
    , prepareInput
    , inputPhraseLengths
    )

import Fortuna
    (inputPerms)

import Cogito
    (makeMusicScore)

import Scribo.MEI
    (score2mei)

data InputMode  = Stdin  | Filein  deriving (Eq)
data OutputMode = Stdout | Fileout deriving (Eq)

-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Output to PDF via Lilypond.
main :: IO ()
main = do
   
    args <- getArgs
    if (length args) /= 2 
        then do 
            putStrLn "Usage: arca [INFILE.xml] [OUTFILE.mei]\n\
                        \ Use '-' for INFILE to read from standard input\n\
                        \ Use '-' for OUTFILE to write to standard output"
            exitFailure
        else do
    
        let
            infileName  = head args
            outfileName = last args
        
            inputMode  | infileName == "-"  = Stdin
                       | otherwise          = Filein

            outputMode | outfileName == "-" = Stdout
                       | otherwise          = Fileout

        rawInput <- if inputMode == Stdin
                        then do 
                            source <- getContents
                            return(source)
                        else do 
                            source <- readFile infileName
                            return(source)

        let 
            input       = readInput rawInput
            sections    = prepareInput input 
            lengths     = inputPhraseLengths sections
            metadata    = arkMetadata input

        perms <- inputPerms lengths

        let 
            score = makeMusicScore arca sections perms 
            mei   = score2mei arca metadata score

        if outputMode == Stdout
            then do putStr mei
            else do
                let outfile | inputMode == Stdin && outputMode == Fileout
                                = "musica.mei"
                            | inputMode == Filein && outputMode == Fileout
                                = dropExtension outfileName ++ ".mei"
                            | otherwise 
                                = outfileName

                writeFile outfile mei
               


