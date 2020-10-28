{-|
 -
Module      : Cogito
Description : Process ark input to create music
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module processes data from the ark to convert it into music (/cogito/,
Latin, "I think").
-}

module Cogito where

import Data.List 
    (transpose)

import Aedifico 
    (Pnum       (..),
     Accid      (Na, AccidNil),
     Octave     (OctNil),
     VoiceName  (..),
     Dur        (Br, BrR),
     Meter,
     Mode       (..),
     Style,
     PenultLength,
     ArkConfig  (..),
     Arca,
     getVoice,
     getRperm)

import Fortuna 
    (Perm (voiceIndex, rhythmIndex), 
     listPerms)

import Lectio

-- * Pitches and Groups of Them

-- | A 'Pitch' stores the essential information for notating a single note.
data Pitch = Pitch {
    pnum  :: Pnum, -- ^ Enum for diatonic pitch number
    oct   :: Int,  -- ^ Helmholtz system, middle C = 4
    dur   :: Dur,  -- ^ Duration, one of @Dur@ enum
    accid :: Accid -- ^ Accidental
} deriving (Show, Eq, Ord)

-- | A 'RawPitch' is the same as a 'Pitch' except instead of a 'Pnum' it has
-- an 'Int'. This allows us to add and subtract from pitches and then use
-- 'stdPitch' to normalize them by adjusting the octave and pitch like the two
-- digits of a base-7 number.
data RawPitch = RawPitch {
    rawPnum    :: Int,
    rawOct     :: Int,
    rawDur     :: Dur,
    rawAccid   :: Accid
} deriving (Show, Eq, Ord)

-- | A 'Voice' is a list of pitches with an identifier for the voice type.
data Voice = Voice {
    voiceID :: VoiceName, -- ^ Enum for Soprano, Alto, Tenor or Bass
    music   :: [Pitch]    
} deriving (Show, Eq, Ord)


-- | Make a list of voices with same 'voiceID' into one 'Voice' (combine the
-- lists of 'Pitch'es into one)
mergeVoices :: [Voice] -> Voice
mergeVoices vs =
    if all (== id) $ map (\ v -> voiceID v) vs
    then
        Voice { 
            voiceID = id,
            music = concat $ map (\ v -> music v) vs
        }
    else error "Voice IDs not equal"
    where
        id = voiceID $ head vs



-- | A 'Chorus' is a group (list) of four 'Voice' items
--
-- __TODO__: We don't actually define it as being four items.
type Chorus = [Voice] 

-- * Manipulating the 'Pitch'

-- | Create a rest (that is, a 'Pitch' with duration only)
--
-- We make a 'Pitch' but set the 'pnum' to 'Rest'; 'oct' and 'accid' are set
-- to special nil values ('OctNil', 'AccidNil')
--
-- __TODO__: We are setting the octave using @fromEnum OctNil@: Isn't this the
-- same as just setting it to zero? Is there a better way to mark this?
newRest :: Dur      -- ^ Rhythmic duration for this note
        -> Pitch    
newRest d = Pitch {
    pnum = Rest,
    oct = (fromEnum OctNil),
    accid = AccidNil,
    dur = d
}

-- | Standardize pitch.
--
-- A 'Pitch' is like a two-digit number in base 7, where the first digit (the
-- "7s") is the octave. If the second digit (the "1s") is over 7, we must
-- increment the octave. A "standardized pitch" in our implementation is one
-- that conforms to this requirement, so the 'stdPitch' method takes all the
-- input for a 'Pitch' and does the necessary conversions to set the octave.
--
-- We need this because Kircher's tables include both pitch number 1 and pitch
-- 8. If pitch input is Kircher's pitch 8, then set pitch num to PcC (0) and
-- add one to octave. 
--
-- __TODO__: We don't check for values out of range because we know what input we
-- are getting from the ark. Is this okay?
stdPitch :: RawPitch -> Pitch
stdPitch pitch1 = 
    if rawPnum pitch1 < 7
        then Pitch {
            pnum    = toEnum $ rawPnum pitch1,
            oct     = rawOct pitch1,
            dur     = oldDur,
            accid   = oldAccid
        }
        else Pitch { 
            pnum    = toEnum $ newPnum,
            oct     = newOct,
            dur     = oldDur,
            accid   = oldAccid
        }
        where
            oldPnum    = rawPnum pitch1
            oldOct     = rawOct pitch1
            oldDur     = rawDur pitch1
            oldAccid   = rawAccid pitch1

            newPnum     = snd pitchDivide
            newOct      = oldOct + fst pitchDivide
            pitchDivide = oldPnum `quotRem` 7

-- | Increment a pitch by increasing its 'Pnum' pitch number and its octave if
-- necessary (using 'Pitch' structure like a base-7 number). Create a new
-- 'Pitch' with incremented 'Pnum' and then standardize it with 'stdPitch' to
-- get correct octave and pitch number.
incPitch :: Pitch -> Pnum -> Pitch
incPitch pitch1 newPnum = stdPitch RawPitch {
    rawPnum  = fromEnum (pnum pitch1) + fromEnum newPnum,
    rawOct   = oct pitch1,
    rawDur   = dur pitch1,
    rawAccid = accid pitch1
}

-- ** Adjust pitch for mode

-- | Pitch number offsets from Kircher's mode table
modeOffset :: Mode -> Pnum
modeOffset mode = case mode of
    Mode1   -> PCd
    Mode2   -> PCg
    Mode3   -> PCa
    Mode4   -> PCa
    Mode5   -> PCb
    Mode6   -> PCf
    Mode7   -> PCg
    Mode8   -> PCg
    Mode9   -> PCd
    Mode10  -> PCa
    Mode11  -> PCc
    Mode12  -> PCf

-- | Adjust a pitch to be in a given mode. We can accomplish the same effect
-- as Kircher's mode tables by just adding an offset for the first note in the
-- mode and adding accidentals to the modes that have them.
pitchInMode :: Pitch -> Mode -> Pitch
pitchInMode pitch mode = incPitch pitch $ modeOffset mode

-- * Match pitches and rhythms 

-- ** Get music data for a single voice

-- | Convert Kircher's 1-indexed pitch numbers to our 0-indexed 'Pnum':
-- subtract 1 and convert to enum
--
-- We don't check for range because we are controlling the data input from
-- Kircher.
toPnum :: Int -- ^ number from Kircher's tables, 1--8
        -> Pnum
toPnum n = toEnum (n - 1)
           
-- | Check to see if a rhythmic duration is a rest type (the rest enums begin
-- with 'BrR' so we compare with that)
isRest :: Dur -> Bool
isRest dur = dur >= BrR 

-- | Take two lists and zip them together, that is, make a new list of ordered
-- pairs constructed from sequential items of the two lists, BUT:
-- if an element in the first list satisfies a given @test@, make a pair of that
-- element and a sustitute element (@sub@) instead of the corresponding
-- element from the second list.
zipFill :: [a]          -- ^ list 1
        -> [b]          -- ^ list 2
        -> (a -> Bool)  -- ^ test
        -> b            -- ^ substitute element to use instead of list 2 item,
                        --    if list 1 item meets test 
        -> [(a, b)]     -- ^ return list of pairs made of list 1 and 2, in
                        --    item order 
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

-- | Make a pitch from duration and pitch number, getting octave based on voice
-- name using 'voice2octave'. If the duration input is a rest type (e.g.,
-- 'BrR'), then make a 'newRest'; otherwise a 'stdPitch'.
--
-- Adjust the pitch for mode (and thereby standardize it) ('pitchInMode').
--
-- __TODO__: This could also be generalized; we are not checking inputs
-- because we control data input.
pair2Pitch :: (Dur, Int) -- ^ duration and pitch number 0-7
            -> VoiceName 
            -> Mode
            -> Pitch
pair2Pitch pair voice mode =
    if isRest thisDur 
        then newRest thisDur
        else pitchInMode newPitch mode
        where
            thisDur  = fst pair
            thisPnum = snd pair
            newPitch = Pitch {
                pnum  = (toPnum thisPnum),
                oct   = voice2octave voice,
                dur   = thisDur,
                accid = Na
            } 


-- | Get the right octave range for each voice type
--
-- __TODO__: replace with something more nuanced, based on Kircher's
-- "palimpsest" approach (using clefs and staff range)
voice2octave :: VoiceName -> Int
voice2octave v = case v of
    Soprano -> 5
    Alto    -> 4
    Tenor   -> 3
    Bass    -> 2

-- | Central functions of the ark: given all parameters required by Kircher
-- (style, meter, syllable count, penultimate syllable length), select a voice
-- permutation (Kircher's number tables) from the appropriate part of the ark
-- and match it to a rhythm permutation (his tables of note values).
--
-- Return a 'Voice' with the pitches for a single voice part.
--
-- We use 'getVoice' and 'getRperm' from the @Aedifico@ module.
--
-- Because the rhythms can include rest, we have to match up pitches and
-- rhythms accordingly using 'zipFill' with the test 'isRest'.
ark2voice :: Arca       -- ^ ark data structure
        -> ArkConfig    -- ^ we pass this along to 'getVoice' and 'getRperm';
                        --      we use the 'Mode' for 'pair2Pitch'
        -> PenultLength -- ^ penultimate syllable length
        -> Int          -- ^ syllable count
        -> VoiceName    -- ^ voice name enum
        -> Perm         -- ^ contains random index for voice and rhythm
                        --      permutation
        -> Voice
ark2voice arca config penult sylCount voice perm =
    Voice { 
        voiceID = voice, 
        music   = map (\ p -> pair2Pitch p voice mode) pairs
    }
        where
            mode        = arkMode config
            pairs       = zipFill rperm vpermVoice isRest (fromEnum Rest) 
            vpermVoice  = getVoice arca config penult sylCount voice vpermNum
            rperm       = getRperm arca config penult sylCount rpermNum
            vpermNum    = voiceIndex perm
            rpermNum    = rhythmIndex perm

-- | Get music data for all four voices and pack them into a 'Chorus'.
--
-- __TODO__: Why do we use a @Fortuna@ 'Perm' here but just two integers in
-- the preceding function? Do we need to have all these parameters for both of
-- these functions?
--
-- We should be getting a new 'Perm' for each Chorus.
getChorus :: Arca       -- ^ ark data structure
        -> ArkConfig    -- ^ we pass this along to 'ark2voice'
        -> Phrase       -- ^ input text processed by @Lectio@
        -> Perm         -- ^ 'Perm' (from @Fortuna@, includes index for voice
                        --       and rhythm)
        -> Chorus
getChorus arca config phrase perm = 
    map (\ v -> ark2voice arca config penult sylCount v perm) 
        [Soprano, Alto, Tenor, Bass]
    where
        penult      = phrasePenultLength phrase
        sylCount    = phraseSylCount phrase

-- | A 'Symphonia' is the amalgamation of a list of 'Chorus'es into one
-- 'Chorus'
type Symphonia = Chorus

-- | To make a 'Symphonia' we take a 'Sentence' and list of 'Perm's, use
-- 'getChorus' to get the ark data for each 'Phrase' in the sentence, each
-- using its own 'Perm'; then we use 'transpose' to reorder the lists. 
--
-- Where we had @[[S1, A1, T1, B1], [S2, A2, T2, B2], [S3, A3, T3, B3]]@
-- we end with @[[S1, S2, S3], [A1, A2, A3], [T1, T2, T3], [B1, B2, B3]]@.
-- A list of four voices becomes four lists of voices.
-- We need to combine each of those voices into a single voice to have a list
-- of four (longer) voices.
--
-- The @Scribo@ module calls this function to get all the ark data needed to
-- set a whole 'Sentence', in the central function of our implementation,
-- @Scribo.compose@.
getSymphonia :: Arca -> Sentence -> [Perm] -> Symphonia
getSymphonia arca sentence perms = 
    map (\ vs -> mergeVoices vs) transposed
    where
        transposed  = transpose choruses
        config      = arkConfig sentence
        choruses    = map (\ i -> getChorus arca config (fst i) (snd i)) permPhrases
        permPhrases = zip (phrases sentence) perms


