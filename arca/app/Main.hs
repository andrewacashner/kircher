{- | 
 - /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
 - Automatically compose music using Kircher's /Arca musarithmica/ from
 - /Musurgia universalis/ (Rome, 1650).
 -
 - Implemented in Haskell by Andrew Cashner, <andrew.cashner@rochester.edu>,
 - 2020
 -}

module Main where

import Arca_musarithmica 
    (arca)

import Aedifico
    (Style      (..),
     Mode       (..),
     Meter      (..), 
     ArkConfig  (..))

import Lectio
    (Sentence (sentenceLength))

import Scribo
    (compose)

import Fortuna
    (listPerms)

import Input.Ps150
    (text)

-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Reading from standard input and writing to standard output for now.
-- Default is Lilypond output.
main :: IO ()
main = do
   
    perms <- listPerms $ sentenceLength text

    let music = compose arca text perms 
    
    putStrLn music


