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

import Arca_musarithmica 
    (arca)

import Aedifico
    (Style      (..),
     Mode       (..),
     Meter      (..), 
     ArkConfig  (..))

import Lectio
    (readInput,
     prepareText,
     Sentence (sentenceLength),
     ArkInput (..))

import Scribo
    (compose)

import Fortuna
    (listPerms)

-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Reading from standard input and writing to standard output for now.
-- Default is Lilypond output.
main :: IO ()
main = do
    
    [infileName, outfileName] <- getArgs
    rawInput <- readFile infileName

    let 
        input    = readInput rawInput
        sections = map (\ s -> prepareText s) $ arkSections input
        sentenceLengths = map (\ ss -> map (\ s -> sentenceLength s) ss) sections

    perms <- mapM (\ phrase -> listPerms phrase) sentenceLengths

    let 
        music = compose arca sections perms 

    writeFile outfileName music

    --writeFile outfileName $ unlines [show input, show sections, show perms]

