{- Scribo: 
 - Writing output of Kircher's Arca to music-notation language
 -}

module Scribo where

import Arca 
import Cogito

-- * Write to Lilypond
-- | Write pitch as Lilypond music note.
-- Look up needed string values for letter name, accidental, octave tick marks,
-- and duration in lists based on data in given @Pitch@.
-- If it is a rest, just print the rest rhythm string.
pitch2ly :: Pitch -> String
pitch2ly (Pitch pnum oct dur accid) =
    if isRest dur
        then duration
        else pitchLetter ++ accidental ++ octaveTicks ++ duration
    where
        pitchLetter = pitch2str pnum
        accidental  = accid2str accid
        octaveTicks = oct2str oct
        duration    = dur2str dur

-- ** Convert individual pitch data members to strings
dur2str :: Dur -> String
dur2str dur = durValues !! fromEnum dur
    where durValues = 
            ["\\breve", "1", "2", "4", "8",
            "\\breve.", "1.", "2.", "4.", "8.",
            "r\\breve", "r1", "r2", "r4", "r8"]

pitch2str :: Pnum -> String
pitch2str pnum = pitchLetter !! fromEnum pnum
    where pitchLetter = words "c d e f g a b"

accid2str :: Accid -> String
accid2str accid = suffix !! fromEnum accid
    where suffix = ["es", "", "is", ""] 

-- | Create tick marks to show octave (Helmholtz oct 4 = c')
oct2str :: Int -> String
oct2str oct  
    | oct < 3   = replicate degree low 
    | oct > 3   = replicate degree high 
    | otherwise = ""
    where 
        low = ','
        high = '\''
        degree = abs (oct - 3)


-- ** Write whole list of @Pitch@ elements to a single Lilypond string
music2ly :: [Pitch] -> String
music2ly music = enbrace (unwords (map pitch2ly music))

enbrace :: String -> String
enbrace str = "{" ++ str ++ "}"

-- | Run the whole machine in one go.
compose :: Arca -> Style -> PenultLength -> Int ->
    Meter -> VoiceName -> Int -> String
compose arca style penult sylCount meter voice i = music2ly music
    where music = getMusic arca style penult sylCount meter voice i




