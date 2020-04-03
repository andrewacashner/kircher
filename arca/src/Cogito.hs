{- |
 Cogito: Processing ark input to create music
-}

module Cogito where

import Arca
import Fortuna
import Lectio

-- * @Pitch@ Datatype
-- | Essential information for notating a single note.
data Pitch = Pitch {
    pnum  :: Pnum, -- Enum for diatonic pitch number
    oct   :: Int,  -- Helmholtz system, middle C = 4
    dur   :: Dur,  -- Duration, one of @Dur@ enum
    accid :: Accid -- Accidental
} deriving (Show, Eq, Ord)

-- * @Voice@ Datatype
-- | A voice is a list of pitches with an identifier for the voice type.
data Voice = Voice {
    voiceID :: VoiceName, -- Enum for Soprano, Alto, Tenor or Bass
    music   :: [Pitch]    -- List of @Pitch@ 
} deriving (Show, Eq, Ord)

-- * @Chorus@ Datatype
-- | A chorus is a group of four voices (list of @Voice@)
type Chorus = [Voice] 

-- ** Working with pitches
-- | Create a rest (that is, a @Pitch@ with only duration)
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

-- | Make a pitch from duration and pnum data, getting octave based on voice
-- name
pair2Pitch :: (Dur, Int) -> VoiceName -> Pitch
pair2Pitch pair voice =
    if isRest thisDur 
        then newRest thisDur
        else stdPitch (toPnum thisPnum) oct thisDur Na
    where
        thisDur  = fst pair
        thisPnum = snd pair
        oct = voice2octave voice

-- | Get the right octave range for each voice type
-- TODO replace with something more nuanced, based on Kircher's "palimpsest"
-- approach (using clefs and staff range)
voice2octave :: VoiceName -> Int
voice2octave v = case v of
    Soprano -> 5
    Alto    -> 4
    Tenor   -> 3
    Bass    -> 3 

-- | Central functions of the ark: given all parameters required by Kircher
-- (style, meter, syllable count, penultimate syllable length), select a voice
-- permutation (Kircher's number tables) from the appropriate part of the ark
-- and match it to a rhythm permutation (his tables of note values).
-- Return a list of pairs, each contain a pitch number and a duration, e.g.
-- @[(5,Sb),(5,Mn)]@
ark2voice :: Arca -> Style -> PenultLength -> Int -> 
    Meter -> VoiceName -> Int -> Int -> Voice
ark2voice arca style penult sylCount meter voice vpermNum rpermNum =
    Voice { 
        voiceID = voice, 
        music = map (\ p -> pair2Pitch p voice) pairs
    }
        where
            vpermVoice = getVoice arca style penult sylCount voice vpermNum
            rperm = getRperm arca style penult sylCount meter rpermNum
            pairs = zipFill rperm vpermVoice isRest (fromEnum Rest) 
            -- getVoice, getRperm in Arca

-- | Get music data for all four voices and pack them into a @Chorus@
getChorus :: Arca -> Style -> Meter -> Perm -> Phrase -> Chorus
getChorus arca style meter perm phrase = 
    map (\ v -> ark2voice arca style penult sylCount meter v vperm rperm) 
        [Soprano, Alto, Tenor, Bass]
    where
        penult      = phrasePenultLength phrase
        sylCount    = phraseSylCount phrase
        vperm       = voice perm
        rperm       = rhythm perm

-- | Flip x and y: Take list [[1, 2], [11, 22], [111, 222]] and 
-- return [[1, 11, 111], [2, 22, 222]]
pivot :: [[a]] -> [[a]]
pivot [] = []
pivot [x:xs] = [x:xs]
pivot ((x:xs):xss) = (x:xs) : pivot xss

-- TODO glue together multiple choruses using pivot
-- make list of all data for phrases, then run machine for each phrase and
-- glue together
--
--compose :: Arca -> Vperm -> Rperm -> Style -> Meter -> Int -> PenultLength
--compose arca vperm rperm style meter syl len =
--- cf compose in Scribo
    


    
