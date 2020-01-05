{- Cogito:
 - Processing ark input to create music
 -}

module Cogito where

import Arca

-- * Enums
-- * @Pitch@ Datatype
-- | Essential information for notating a single note.
data Pitch = Pitch {
    pnum  :: Pnum, -- Enum for diatonic pitch number
    oct   :: Int,  -- Helmholtz system, middle C = 4
    dur   :: Dur,  -- Duration, one of @Dur@ enum
    accid :: Accid -- Accidental
} deriving (Show, Eq, Ord)

newRest :: Dur -> Pitch
newRest d = Pitch {
    pnum = Rest,
    oct = (fromEnum OctNil),
    accid = AccidNil,
    dur = d
}

-- | Standardize pitch. If pitch input is Kircher's pitch 8, then set pitch
-- num to PCc (0) and add one to octave. Sort of like a base-7 conversion.
stdPitch :: Pnum -> Int -> Dur -> Accid -> Pitch
stdPitch pnum oct dur accid 
    | pnum == PCc8 = Pitch { 
        pnum = PCc, 
        oct = oct + 1,
        dur = dur,
        accid = accid
    }
    | otherwise = Pitch pnum oct dur accid

-- * Match pitches and rhythms 
--
-- ** Get music data for a single voice

toPnum :: Int -> Pnum
toPnum n = toEnum (n - 1)
-- TODO account for Kircher's pitch #8
           
-- | Check to see if a rhythmic duration is a rest type
isRest :: Dur -> Bool
isRest dur = dur >= BrR 

-- | Take two lists and zip them together, that is, make a new list of ordered
-- pairs constructed from sequential items of the two lists, BUT:
-- if an element in the first list satisfies a given @test@, make a pair of that
-- element and a sustitute element (@sub@) instead of the corresponding
-- element from the second list.
zipFill [] _ test sub = [] 
    -- stop when the first list is done
zipFill a [] test sub = zipFill a [sub] test sub
    -- when the second list is done, use @sub@ as placeholder for remaining
    -- items in first list
zipFill (a:as) (b:bs) test sub = 
    if test a 
        then (a, sub) : zipFill as (b:bs) test sub 
        else (a, b) : zipFill as bs test sub
    -- build a list of pairs of either the heads of both lists or the head
    -- of the first list and the @sub@ value

pair2Pitch :: (Dur, Int) -> Pitch
pair2Pitch pair  =
    if isRest thisDur 
        then newRest thisDur
        else stdPitch (toPnum thisPnum) 4 thisDur Na
            -- TODO set octave per voice
    where
        thisDur  = fst pair
        thisPnum = snd pair

-- | Central function of the ark: given all parameters required by Kircher
-- (style, meter, syllable count, penultimate syllable length), select a voice
-- permutation (Kircher's number tables) from the appropriate part of the ark
-- and match it to a rhythm permutation (his tables of note values).
-- Return a list of pairs, each contain a pitch number and a duration, e.g.
-- @[(5,Sb),(5,Mn)]@

getMusic :: Arca -> Style -> PenultLength -> Int -> 
    Meter -> VoiceName -> Int -> [Pitch]
getMusic arca style penult sylCount meter voice i =
    map pair2Pitch pairs
        where
            vpermVoice = getVoice arca style penult sylCount voice i
            rperm = getRperm arca style penult sylCount meter i
            pairs = zipFill rperm vpermVoice isRest (fromEnum Rest) 
 
