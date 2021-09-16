{-|
Module      : Scribo.Lilypond
Description : Write output of the ark to Lilypond music-notation language
Copyright   : (c) 2021
Stability   : Experimental

This module is our implementation of Kircher's /palimpsest phonotacticum/, his
system for writing out the music created using the ark.  Certain elements that
Kircher used notation to calculate (like determining vocal ranges by clef
combinations and the size of the staff) we actually do in the @Cogito@ module.
This module is purely focused on output of complete music information to a
music-notation language.

This module outputs to Lilypond, which could then be processed by that program
to PDF, MIDI, or other formats. 

This is a stub of what used to be a full Lilypond writing module (now archived
in test/). We just use this for testing.
-}


module Scribo.Lilypond where

import Aedifico
    ( Pnum       (..)
    , Accid      (..)
    , Dur        (..)
    , Pitch      (..)
    )

import Cogito.Musarithmetic
    ( isRest )

-- * Write individual data types to Lilypond strings

-- | Write pitch as Lilypond music note.
-- Look up needed string values for letter name, accidental, octave tick marks,
-- and duration in lists based on data in given 'Pitch'.
-- If it is a 'Aedifico.Pnum.Rest', just print the rest rhythm string.
--
-- Most of these just require using an enum value as index to a list of
-- strings or characters. The octave requires us to calculate the number of
-- commas or apostrophes to add (relative to Helmholtz octave 3 = @c@).
pitch2ly :: Pitch -> String
pitch2ly p =
    if isRest $ dur p
        then duration
        else if accid p /= Na
            then "\\ficta " ++ pitchLetter ++ accidental ++ octaveTicks ++ duration
            else pitchLetter ++ octaveTicks ++ duration
    where
        duration    = case (dur p) of
            Br  -> "\\breve"
            Sb  -> "1"
            Mn  -> "2"
            Sm  -> "4"
            Fs  -> "8"
            BrD -> "\\breve."
            SbD -> "1."
            MnD -> "2."
            SmD -> "4."
            FsD -> "8."
            LgR -> "r\\longa"
            BrR -> "r\\breve"
            SbR -> "r1"
            MnR -> "r2"
            SmR -> "r4"
            FsR -> "r8"

        pitchLetter = case (pnum p) of
            PCc  -> "c"
            PCd  -> "d"
            PCe  -> "e"
            PCf  -> "f"
            PCg  -> "g"
            PCa  -> "a"
            PCb  -> "b"
            PCc8 -> "c"

        accidental  = case (accid p) of
            Fl  -> "es"
            Na  -> ""
            Sh  -> "is"
            AccidNil -> ""

        octaveTicks = oct2str $ oct p

        oct2str :: Int -> String
        oct2str oct  
            | oct < 3   = replicate degree low 
            | oct > 3   = replicate degree high 
            | otherwise = ""
            where 
                low = ','
                high = '\''
                degree = abs (oct - 3)


