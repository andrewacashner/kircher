{- Scribo: 
 - Writing output of Kircher's Arca to music-notation language
 -}

module Scribo where

import Arca 
    (Dur)
import Cogito

-- * Write to Lilypond
--
-- | Write pitch as Lilypond music note.
-- Look up needed string values for letter name, accidental, octave tick marks,
-- and duration in lists based on data in given @Pitch@.
pitch2ly :: Pitch -> String
pitch2ly (Pitch pnum oct dur accid) = 
    pitchLetter : accidental ++ octaveTicks ++ duration
    where
        pitchLetter = "cdefgabC" !! fromEnum pnum
        accidental  = ["es", "", "is", "_"] !! fromEnum accid

        octaveTicks | oct < 3 = replicate degree low
                    | oct > 3 = replicate degree high
                    | otherwise = ""
        low = ','
        high = '\''
        degree = abs (oct - 3)

        duration = durValues !! fromEnum dur
        durValues = 
            ["\\breve", "1", "2", "4", "8",
             "\\breve.", "1.", "2.", "4.", "8.",
             "r\\breve", "r1", "r2", "r4", "r8"]
        







