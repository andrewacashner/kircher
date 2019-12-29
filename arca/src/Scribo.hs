{- Scribo: 
 - Writing output of Kircher's Arca to music-notation language
 -}

module Scribo where

import Arca 
    (Dur)

-- * Enums
-- | 0-indexed diatonic pitch-class number
data Pnum = PCc | PCd | PCe | PCf | PCg | PCa | PCb
    deriving (Show, Enum)

-- | Accidentals
data Accid = 
    Fl      -- flat
    | Na    -- natural
    | Sh    -- sharp
    deriving (Show, Enum)

-- * @Pitch@ Datatype
-- | This type records the essential information for notation a single note.
data Pitch = Pitch {
    pnum  :: Pnum, -- Enum for diatonic pitch number
    oct   :: Int,  -- Helmholtz system, middle C = 4
    dur   :: Dur,  -- Duration, one of @Dur@ enum
    accid :: Accid -- Accidental
} deriving (Show)
-- ^ TODO: add syllables

-- -- * @Rest@ Datatype
-- -- | Like @Pitch@ but just rhythmic information
-- data Rest = Rest {
--     dur :: Dur
-- } deriving (Show)

    
-- * Write to Lilypond
--
-- | Write pitch as Lilypond music note.
-- Look up needed string values for letter name, accidental, octave tick marks,
-- and duration in lists based on data in given @Pitch@.
pitch2ly :: Pitch -> String
pitch2ly (Pitch pnum oct dur accid) = 
    pitchLetter : accidental ++ octaveTicks ++ duration
    where
        pitchLetter = "cdefgab" !! fromEnum pnum
        accidental  = ["es", "", "is"] !! fromEnum accid

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
        







