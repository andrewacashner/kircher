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

-- * Match pitches and rhythms 
--
-- ** Get music data for a single voice

toPnum :: Int -> Pnum
toPnum n = toEnum (n - 1)
-- TODO account for Kircher's pitch #8


-- | Central function of the ark: given all parameters required by Kircher
-- (style, meter, syllable count, penultimate syllable length), select a voice
-- permutation (Kircher's number tables) from the appropriate part of the ark
-- and match it to a rhythm permutation (his tables of note values).
-- Return a list of pairs, each contain a pitch number and a duration, e.g.
-- @[(5,Sb),(5,Mn)]@

getMusic :: Arca -> Style -> PenultLength -> Int -> 
    Meter -> VoiceName -> Int -> [Pitch]
getMusic arca style penult sylCount meter voice i =
    map (\ pair -> Pitch { 
            pnum = toPnum (fst pair),
            oct = 4, -- TODO for now
            accid = Na,
            dur = snd pair 
        } 
    ) pairs
        where
            vpermVoice = getVoice arca style penult sylCount voice i
            rperm = getRperm arca style penult sylCount meter i
            pairs = zip vpermVoice rperm
            
            -- TODO check for rests in getMusic

{-
 - if rperm[i] is a rest rhythm type, then join it to a rest;
 - advance to rperm[i+1] but stay with vperm[i] for next pair
 -}

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
        then (a, b) : zipFill as bs test sub 
        else (a, sub) : zipFill as (b:bs) test sub
    -- build a list of pairs of either the heads of both lists or the head
    -- of the first list and the @sub@ value

-- TODO
-- build a list of Pitch or Rest types using something like zipFill with
-- isRest as the test. But instead of just making pairs, create Pitch or Rest
-- types. How to have it return either kind?


