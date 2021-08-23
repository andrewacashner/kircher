{-|
Module      : Cogito.Musarithmetic
Description : Data structures and functions to adjust and store the ark's music output
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module provides the tools used in the main @Cogito@ module to adjust
music created by the ark and to store it in internal structures that will then
be used by the @Scribo@ modules.
-}

module Cogito.Musarithmetic where

import Data.List 
    (findIndex)

import Data.Maybe
    (fromJust)

import Aedifico 
    ( 
      (!!?)
    , Accid        (..)
    , AccidType    (..)
    , Arca         (..)
    , ArkConfig    (..)
    , Dur          (..)
    , Mode         (..)
    , ModeList
    , ModeSystem
    , Octave       (OctNil)
    , Pnum         (..)
    , System       (..)
    , VoiceName    (..)
    , VoiceRanges
    , TextMeter    (..)
    , Pitch        (..)
    , PnumAccid
    , getVectorItem
    , simplePitch
    , modeOrModeB
    )


-- * Pitches and Groups of Them

-- | A 'RawPitch' is the same as a 'Pitch' except instead of a 'Pnum' it has
-- an 'Int'. This allows us to add and subtract from pitches and then use
-- 'stdPitch' to normalize them by adjusting the octave and pitch like the two
-- digits of a base-7 number.
data RawPitch = RawPitch {
    rawPnum    :: Int,
    rawOct     :: Int,
    rawDur     :: Dur,
    rawAccid   :: Accid,
    rawAccidType :: AccidType
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
-- __TODO__: Do we still need this with new MEI setup?
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
    dur = d,
    oct = (fromEnum OctNil),
    accid = AccidNil,
    accidType = None
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
    let oldPnum = rawPnum pitch1
    in
    if oldPnum >= 0 && oldPnum < 7
        then Pitch {
            pnum      = toEnum $ rawPnum pitch1,
            oct       = rawOct pitch1,
            dur       = oldDur,
            accid     = oldAccid,
            accidType = oldAccidType
        }
        else Pitch { 
            pnum      = toEnum $ newPnum,
            oct       = newOct,
            dur       = oldDur,
            accid     = oldAccid,
            accidType = oldAccidType
        }
        where
            oldPnum      = rawPnum pitch1
            oldOct       = rawOct pitch1
            oldDur       = rawDur pitch1
            oldAccid     = rawAccid pitch1
            oldAccidType = rawAccidType pitch1

            newPnum     = snd pitchDivide
            newOct      = oldOct + fst pitchDivide
            pitchDivide = oldPnum `divMod` 7

-- | Increment a pitch by increasing its 'Pnum' pitch number and its octave if
-- necessary (using 'Pitch' structure like a base-7 number). Create a new
-- 'Pitch' with incremented 'Pnum' and then standardize it with 'stdPitch' to
-- get correct octave and pitch number.
incPitch :: Pitch -> Int -> Pitch
incPitch pitch1 newPnum = stdPitch RawPitch {
    rawPnum      = fromEnum (pnum pitch1) + newPnum,
    rawOct       = oct pitch1,
    rawDur       = dur pitch1,
    rawAccid     = accid pitch1,
    rawAccidType = accidType pitch1
}

-- ** Adjust pitch for mode

-- | Is a mode in /cantus mollis/? Should there be a flat in the key
-- signature?
modeMollis :: Mode -> ModeSystem  -> Bool
modeMollis mode systems =
    let s = getVectorItem "modeMollis:systems" systems $ fromEnum mode
    in case s of
        Durus  -> False
        Mollis -> True

-- | Adjust a pitch to be in a given mode. 
pnumAccidInMode :: Int -> ModeList -> Mode -> PnumAccid
pnumAccidInMode rawPnum modeList mode = pnum
    where 
        pnum        = getVectorItem "pnumAccidInMode:pnum" modeScale rawPnum
        modeScale   = getVectorItem "pnumAccidInMode:modeScale" modeList $ fromEnum mode
  
-- | Get the modal final for this mode. What pitch = 0 in this mode? (In
-- Kircher's 1-indexed vperms, the final is 1 or 8.)
modalFinal :: ModeList -> Mode -> Pitch
modalFinal modeList mode = simplePitch (pnum, 0)
    where 
        pnum      = fst finalPair
        finalPair = getVectorItem "modalFinalInRange:finalPair" modeScale 0
        modeScale = getVectorItem "modalFinalInRange:modeScale" modeList $ fromEnum mode

-- | Get the modal final within range for this voice.
-- What is the lowest octave of that pitch permissible in the range for that
-- voice?
-- Set all the other pitches with reference to that, within the octave between
-- 0--7
modalFinalInRange :: ModeList -> Mode -> VoiceName -> VoiceRanges -> Pitch
modalFinalInRange modeList mode voiceName ranges = 
    adjustPitchInRange basePitch voiceName ranges
    where basePitch = modalFinal modeList mode

-- | What octave is the modal final in for this voice's range?
modalOctaveBase :: ModeList -> Mode -> VoiceName -> VoiceRanges -> Int
modalOctaveBase modeList mode voiceName ranges = 
    oct $ modalFinalInRange modeList mode voiceName ranges


-- | Check to see if a rhythmic duration is a rest type (the rest enums begin
-- with 'LgR' so we compare with that)
isRest :: Dur -> Bool
isRest dur = dur >= LgR 

-- | Is the 'Pitch' a rest?
isPitchRest :: Pitch -> Bool
isPitchRest p = pnum p == Rest



-- ** Measure distances between notes and correct bad intervals
-- *** Convert between diatonic and chromatic pitches to calculate intervals
-- | Convert 'Pitch' to absolute pitch number
absPitch :: Pitch -> Int
absPitch p 
    | isPitchRest p = error "Can\'t convert Rest to absolute pitch" 
    | otherwise     = oct12 + pnum12 + accid12
    where
        oct12   = oct p * 12
        pnum12  = dia2chrom $ pnum p
        accid12 = (fromEnum $ accid p) - 2


-- | Get chromatic offset from C for diatonic pitch classes
dia2chrom :: Pnum -> Int
dia2chrom n = case n of
    PCc  -> 0
    PCd  -> 2
    PCe  -> 4
    PCf  -> 5
    PCg  -> 7
    PCa  -> 9
    PCb  -> 11
    PCc8 -> 12
    _    -> error $ "Unknown pitch class" ++ show n

-- | Are two 'Pitch'es the same chromatic pitch, enharmonically equivalent?
pEq :: Pitch -> Pitch -> Bool
pEq p1 p2   = absPitch p1 == absPitch p2

-- | Pitch greater than?
pGt p1 p2   = absPitch p1 >  absPitch p2

-- | Pitch less than?
pLt p1 p2   = absPitch p1 <  absPitch p2

-- | Pitch greater than or equal?
pGtEq p1 p2 = absPitch p1 >= absPitch p2

-- | Pitch less than or equal?
pLtEq p1 p2 = absPitch p1 <= absPitch p2

-- | Difference between pitches, chromatic interval
p12diff :: Pitch -> Pitch -> Int
p12diff p1 p2
    | isPitchRest p1 || isPitchRest p2
        = 0
    | otherwise 
        = absPitch p1 - absPitch p2

-- | 'p12diff' modulo 12 (= chromatic difference within one octave)
p12diffMod :: Pitch -> Pitch -> Int
p12diffMod p1 p2 = p12diff p1 p2 `mod` 12

-- | Absolute diatonic pitch (octave + pitch)
absPitch7 :: Pitch -> Int
absPitch7 p = (oct p * 7) + (fromEnum $ pnum p)

-- | Difference between pitches, diatonic interval
-- Unison = 0, therefore results of this function are one less than the verbal
-- names of intervals (@p7diff = 4@ means a fifth)
p7diff :: Pitch -> Pitch -> Int
p7diff p1 p2
    | isPitchRest p1 || isPitchRest p2
        = 0
    | otherwise 
        = absPitch7 p1 - absPitch7 p2

-- | Diatonic difference between pitch classes (= pitch difference as though
-- within a single octave); result is 0-indexed, so the interval of a "third"
-- in speech has a @p7diffMod@ of 2
p7diffMod :: Pitch -> Pitch -> Int
p7diffMod p1 p2 = (p7diff p1 p2) `mod` 7

-- | Copy a 'Pitch' (unchanged if 'Rest'), with given function applied to the
-- octave member
octaveAdjust :: Pitch -> (Int -> Int) -> Pitch
octaveAdjust p fn 
    | isPitchRest p = p
    | otherwise     = Pitch { 
        pnum      = pnum p,
        oct       = fn $ oct p,
        accid     = accid p,
        accidType = accidType p,
        dur       = dur p
    }

-- | Raise the octave by 1
octaveUp :: Pitch -> Pitch
octaveUp p = octaveAdjust p (\p -> p + 1)

-- | Lower the octave by 1
octaveDown :: Pitch -> Pitch
octaveDown p = octaveAdjust p (\p -> p - 1)

-- | Is the pitch below the bottom limit of the voice range?
pitchTooLow :: Pitch -> VoiceName -> VoiceRanges -> Bool
pitchTooLow pitch voice ranges = pitch `pLt` lowRange voice ranges

-- | Is the pitch above the upper limit of the voice range?
pitchTooHigh :: Pitch -> VoiceName -> VoiceRanges -> Bool
pitchTooHigh pitch voice ranges = pitch `pGt` highRange voice ranges

-- | Get the bottom limit of the voice range
lowRange :: VoiceName -> VoiceRanges -> Pitch
lowRange voice ranges = fst $ fromJust $ ranges !!? fromEnum voice

-- | Get the top limit of the voice range
highRange :: VoiceName -> VoiceRanges -> Pitch
highRange voice ranges = snd $ fromJust $ ranges !!? fromEnum voice

-- | Adjust a pitch to be in the correct voice range (using @Aedifico.vocalRanges@).
-- If it's in the right range for the voice, leave it alone; if it's too low
-- raise it by an octave, or vice versa if it's too high; keep shifting
-- octaves till it's in range.
adjustPitchInRange :: Pitch -> VoiceName -> VoiceRanges -> Pitch
adjustPitchInRange pitch voice ranges
    | isPitchRest pitch 
        = pitch
    | pitchTooLow pitch voice ranges 
        = adjustPitchInRange (octaveUp pitch) voice ranges
    | pitchTooHigh pitch voice ranges
        = adjustPitchInRange (octaveDown pitch) voice ranges
    | otherwise = pitch

-- | Is the 'Pitch' within the proper range for its voice?
isPitchInRange :: Pitch -> VoiceName -> VoiceRanges -> Bool
isPitchInRange pitch voice ranges = isPitchRest pitch ||
    ((not $ pitchTooLow pitch voice ranges) && 
     (not $ pitchTooHigh pitch voice ranges))

-- ** Adjust list of pitches to avoid bad intervals

-- | Go through list of pitches and reduce intervals that are too large
stepwise :: [Pitch] -> [Pitch]
stepwise []         = []
stepwise (a:[])     = [a]
stepwise (a:b:[])   = [a, (unleap a b)]
stepwise (a:b:c:cs) =
    -- If b is a rest, calculate interval between a and b, adjust the next item
    -- c as though it was preceded by a
    if isPitchRest b
    then 
        let c2 = unleap a c
        in (a:b:(stepwise (c2:cs))) 
    else
        let b2 = unleap a b
        in a:(stepwise (b2:c:cs))

-- | Adjust a whole 'Voice' stepwise
stepwiseVoice :: Voice -> Voice
stepwiseVoice v = Voice {
    voiceID = voiceID v,
    music   = stepwise $ music v
}

-- | No leaps bigger than this interval
-- This is based on 0 as unison so a _maxLeap of 4 is a fifth
_maxLeap = 4 :: Int 

-- | Reduce leap of more than a '_maxLeap' by shifting octave of second note
-- up or down until the interval is within range
--
-- Octave leaps are okay, though.
--
-- __TODO__ : 
--    - but what if after adjusting for leaps, a note is out of range?
--    - and what if there is a descending scale that goes out of range and the
--    only way to adjust it is to make a seventh? need to adjust a whole
--    phrase
--    - But this may be going beyond what Kircher specifies as fully-automated
--    algorithms. 
unleap :: Pitch -> Pitch -> Pitch
unleap p1 p2
    | p7diff p1 p2 == 8
        = p2
    | p7diff p1 p2 > _maxLeap
        = unleap p1 $ octaveUp p2
    | p7diff p1 p2 < (0 - _maxLeap)
        = unleap p1 $ octaveDown p2
    | otherwise
        = p2

-- | Return the highest pitch in a list of pitches.
pitchMax :: [Pitch] -> Maybe Pitch
pitchMax ps = ps !!? maxIndex
    where
        maxIndex  = fromJust $ findIndex 
                    (\p -> (not . isPitchRest) p && absPitch p == maxInt) ps
        maxInt    = maximum pitchInts
        pitchInts = map absPitch $ filter (not . isPitchRest) ps

-- | Return the lowest pitch in a list of pitches.
pitchMin :: [Pitch] -> Maybe Pitch
pitchMin ps = ps !!? minIndex
    where
        minIndex  = fromJust $ findIndex 
                    (\p -> (not . isPitchRest) p && absPitch p == minInt) ps
        minInt    = minimum pitchInts
        pitchInts = map absPitch $ filter (not . isPitchRest) ps
-- | TODO write your own max/min functions for pitches that ignore Rests

-- | Go through list of pitches and reduce intervals that are too large
stepwiseInRange :: [Pitch] -> VoiceName -> VoiceRanges -> [Pitch]
stepwiseInRange [] _ _ = []
stepwiseInRange (a:[]) _ _ = [a]
stepwiseInRange (a:b:[]) _ _ = [a, (unleap a b)]
stepwiseInRange (a:b:c:cs) voice ranges =
    -- If b is a rest, calculate interval between a and b, adjust the next item
    -- c as though it was preceded by a
    if isPitchRest b
    then 
        let c2 = unleap a $ c
        in (a:b:(stepwiseInRange (c2:cs) voice ranges)) 
    else
        let b2 = unleap a b
        in
            -- If the adjusted second pitch b2 is out of range, then we need
            -- to adjust the octave of a accordingly and start over;
            -- If not, continue with a, b2, rest of list
            if pitchTooLow b2 voice ranges
            then stepwiseInRange ((octaveUp a):b:c:cs) voice ranges
            else if pitchTooHigh b2 voice ranges
            then stepwiseInRange ((octaveDown a):b:c:cs) voice ranges
            else a:(stepwiseInRange (b2:c:cs) voice ranges)


-- | Adjust a whole 'Voice' to be in range: check the highest and lowest notes
-- in the list, compare to the range for the voice, and shift the whole thing
-- by octave until all are in range; return error if it can't be done
voiceInRange :: VoiceRanges -> Voice -> Voice
voiceInRange ranges voice
    | all (\p -> isPitchInRange p voiceName ranges) notes
        = voice
    | (tooLow && tooHighAfterAdjust) || (tooHigh && tooLowAfterAdjust)
        = voice
    | tooLow
        = voiceInRange ranges $ Voice voiceName $ map octaveUp notes
    | tooHigh
        = voiceInRange ranges $ Voice voiceName $ map octaveDown notes
    | otherwise 
        = voice
    where
        notes     = music voice 
        voiceName = voiceID voice
        max       = fromJust $ pitchMax notes
        min       = fromJust $ pitchMin notes
        tooHigh   = pitchTooHigh max voiceName ranges
        tooLow    = pitchTooLow min voiceName ranges
        tooHighAfterAdjust = pitchTooHigh (octaveUp max) voiceName ranges
        tooLowAfterAdjust  = pitchTooLow (octaveDown min) voiceName ranges

-- | Adjust a whole 'Voice' stepwise
stepwiseVoiceInRange :: VoiceRanges -> Voice -> Voice
stepwiseVoiceInRange ranges v = Voice {
    voiceID = voiceID v,
    music   = stepwiseInRange (music v) (voiceID v) ranges
}


-- * Data structures to store music composed by the ark

-- | A 'Note' contains a pitch and a syllable, equivalent to MEI @note@
data Note = Note {
    notePitch :: Pitch,
    noteSyllable :: Syllable
} deriving (Show, Eq, Ord)

-- | A 'Syllable' is a single syllable to be paired with a 'Pitch', including
-- its position in the word.
data Syllable = Syllable {
    sylText :: String,
    sylPosition :: SyllablePosition
} deriving (Show, Eq, Ord)

-- | What is the position of the syllable relative to the word? Beginning,
-- middle, or end? This determines hyphenation.
data SyllablePosition =   First 
                        | Middle
                        | Last
                        | Only
                        | Tacet -- ^ no syllable
                        deriving (Show, Enum, Eq, Ord)

-- | A 'MusicPhrase' contains all the notes set using one permutation drawn
-- from the ark, for a single voice.
data MusicPhrase = MusicPhrase {
    phraseVoiceID :: VoiceName,
    notes :: [Note]
} deriving (Show, Eq, Ord)

-- | A list of 'MusicPhrase' items
type MusicSentence  = [MusicPhrase]

-- | A 'MusicSection' contains all the music for one section in the input XML
-- document, for a single voice, together with the parameters set in the input
-- file.
data MusicSection = MusicSection {
    secVoiceID :: VoiceName,
    secConfig :: ArkConfig,
    secSentences :: [MusicSentence]
}

-- | A 'MusicChorus' is a four-voice SATB structure of 'MusicSection' data.
-- __TODO__ do we really need it to be structured this way?
data MusicChorus = MusicChorus {
    soprano :: MusicSection,
    alto    :: MusicSection,
    tenor   :: MusicSection,
    bass    :: MusicSection
}

-- | The full 'MusicScore' is a list of SATB 'MusicChorus' structures.
type MusicScore     = [MusicChorus]


