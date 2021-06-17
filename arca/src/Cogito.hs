{-|
Module      : Cogito
Description : Process ark input to create music
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module processes data from the ark to convert it into music (/cogito/,
Latin, "I think").

= Overview

This module receives input from the @Lectio@ module in the form of a single list of one or more 'LyricSection's, which contain the parsed text to be set to music and the parameters for setting it.

The module uses Kircher's rules to pull the appropriate data from the Arca musarithmica, that is, from the 'Arca' built by the @Aedifico@ module.
It uses the @Fortuna@ module to get lists of random permutation indices.

The main function is 'getSymphonia', which applies all the necessary rules to select music data from the ark for each phrase of text, using the random permutations when a free choice would otherwise be needed. It takes the numerals and rhythmic symbols from Kircher's /pinakes/ (rods); converts the numerals to pitches according to the mode, and combines the pitches and rhythms (accounting for rests as well).
-}

module Cogito where

import Data.List 
    (
        transpose,
        findIndex
    )

import Data.Vector 
    (
        (!),
        (!?),
        fromList
    )

import Data.Maybe
    (
        fromJust,
        fromMaybe
    )

import Aedifico 
    (
        Pnum         (..),
        Accid        (..),
        Octave       (OctNil),
        VoiceName    (..),
        VoiceRanges, 
        Dur          (..),
        Mode         (..),
        Style        (..),
        TextMeter    (..),
        PenultLength (..),
        ArkConfig    (..),
        Arca         (..),
        System       (..),
        ModeSystem,  
        Pitch        (..),
        PnumAccid,
        ModeList,
        getVoice,
        getRperm,
        proseMeter
    )

import Fortuna 
    (
        Perm (voiceIndex, rhythmIndex),
        SectionPerm, 
        SentencePerm
    )

import Lectio

-- * Pitches and Groups of Them

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
    let oldPnum = rawPnum pitch1
    in
    if oldPnum >= 0 && oldPnum < 7
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
            pitchDivide = oldPnum `divMod` 7

-- | Increment a pitch by increasing its 'Pnum' pitch number and its octave if
-- necessary (using 'Pitch' structure like a base-7 number). Create a new
-- 'Pitch' with incremented 'Pnum' and then standardize it with 'stdPitch' to
-- get correct octave and pitch number.
incPitch :: Pitch -> Int -> Pitch
incPitch pitch1 newPnum = stdPitch RawPitch {
    rawPnum  = fromEnum (pnum pitch1) + newPnum,
    rawOct   = oct pitch1,
    rawDur   = dur pitch1,
    rawAccid = accid pitch1
}

-- ** Adjust pitch for mode

-- | Is a mode in /cantus mollis/? Should there be a flat in the key
-- signature?
modeMollis :: Mode -> ModeSystem  -> Bool
modeMollis mode systems =
    let s = systems ! fromEnum mode
    in case s of
        Durus  -> False
        Mollis -> True

-- | Adjust a pitch to be in a given mode. 
pnumAccidInMode :: Int -> Mode -> ModeList -> PnumAccid
pnumAccidInMode rawPnum mode modeList = modeList ! fromEnum mode ! rawPnum
  
-- | Get the modal final within range for this voice.
-- What pitch = 0 in this mode?
-- What is the lowest octave of that pitch permissible in the range for that
-- voice?
-- Set all the other pitches with reference to that, within the octave between
-- 0--7
modalFinalInRange :: Mode -> ModeList -> VoiceName -> VoiceRanges -> Pitch
modalFinalInRange mode modeList voiceName ranges = 
    adjustPitchInRange (Pitch pnum 0 DurNil Na) voiceName ranges
    where 
        pnum = fst $ modeList ! fromEnum mode ! 0

-- | What octave is the modal final in for this voice's range?
modalOctaveBase :: Mode -> ModeList -> VoiceName -> VoiceRanges -> Int
modalOctaveBase mode modeList voiceName ranges = 
    oct $ modalFinalInRange mode modeList voiceName ranges

-- | Adjust the accidental either toward flats or toward sharps, within the
-- 'Accid' enum. If the accidental is unset we just return the original pitch.
accidentalShift :: Pitch 
                -> Accid
                -> Pitch
accidentalShift pitch direction =
    if accid pitch == AccidNil
    then pitch
    else if newAccidNum < fromEnum FlFl || newAccidNum > fromEnum ShSh
    then error "Cannot adjust accidental further"
    else Pitch { 
        pnum  = pnum pitch, 
        oct   = oct pitch, 
        dur   = dur pitch, 
        accid = toEnum newAccidNum
    }
    where
        newAccidNum = operation (fromEnum $ accid pitch) 1
        operation = case direction of
            Fl -> (-)
            Sh -> (+)

-- | Lower a pitch a semitone, but not past 'FlFl'
flatten :: Pitch -> Pitch
flatten pitch = accidentalShift pitch Fl

-- | Raise a pitch a semitone, but not past 'ShSh'
sharpen :: Pitch -> Pitch
sharpen pitch = accidentalShift pitch Sh

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
dia2chrom n = [0, 2, 4, 5, 7, 9, 11, 12] !! fromEnum n 

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

-- | Absolute diatonic pitch (octave + pitch)
absPitch7 :: Pitch -> Int
absPitch7 p = (oct p * 7) + (fromEnum $ pnum p)

-- | Difference between pitches, diatonic interval
p7diff :: Pitch -> Pitch -> Int
p7diff p1 p2
    | isPitchRest p1 || isPitchRest p2
        = 0
    | otherwise 
        = absPitch7 p1 - absPitch7 p2

-- | Copy a 'Pitch' (unchanged if 'Rest'), with given function applied to the
-- octave member
octaveAdjust :: Pitch -> (Int -> Int) -> Pitch
octaveAdjust p fn 
    | isPitchRest p = p
    | otherwise     = Pitch { 
        pnum    = pnum p,
        oct     = fn $ oct p,
        accid   = accid p,
        dur     = dur p
    }

-- | Raise the octave by 1
octaveUp :: Pitch -> Pitch
octaveUp p = octaveAdjust p (\p -> p + 1)

-- | Lower the octave by 1
octaveDown :: Pitch -> Pitch
octaveDown p = octaveAdjust p (\p -> p - 1)

-- * Match pitches and rhythms 

-- ** Get music data for a single voice

-- | Check to see if a rhythmic duration is a rest type (the rest enums begin
-- with 'BrR' so we compare with that)
isRest :: Dur -> Bool
isRest dur = dur >= BrR 

-- | Is the 'Pitch' a rest?
isPitchRest :: Pitch -> Bool
isPitchRest p = pnum p == Rest

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
-- Adjust the pitch for mode (and thereby standardize it) ('pnumAccidInMode').
-- Adjust the octave to put the pitch in the right range for the voice
-- ('adjustPitchInRange').
--
-- __TODO__: This could also be generalized; we are not checking inputs
-- because we control data input.
pair2Pitch :: (Dur, Int) -- ^ duration and pitch number 0-7
            -> VoiceName 
            -> VoiceRanges
            -> Mode
            -> ModeList
            -> Pitch
pair2Pitch pair voice ranges mode modeList =
    if isRest thisDur 
        then newRest thisDur
        else adjustPitchInRange pitch voice ranges
        where
            pitch = stdPitch RawPitch {
                rawPnum    = fromEnum $ fst modePitch,
                rawAccid   = snd modePitch,
                rawOct     = oct $ pitchOffsetFromFinal,
                rawDur     = thisDur
            } 
            modePitch = pnumAccidInMode thisPnum mode modeList
            thisPnum  = (snd pair) - 1 -- adjust to 0 index
            thisDur   = fst pair

            final     = modalFinalInRange mode modeList voice ranges
            pitchOffsetFromFinal = final `incPitch` thisPnum


-- | Get the right starting octave range for each voice type voice2octave :: VoiceName -> Int
voice2octave v = case v of
    Soprano -> 4
    Alto    -> 3
    Tenor   -> 3
    Bass    -> 2

-- | Is the pitch below the bottom limit of the voice range?
pitchTooLow :: Pitch -> VoiceName -> VoiceRanges -> Bool
pitchTooLow pitch voice ranges = pitch `pLt` lowRange voice ranges

-- | Is the pitch above the upper limit of the voice range?
pitchTooHigh :: Pitch -> VoiceName -> VoiceRanges -> Bool
pitchTooHigh pitch voice ranges = pitch `pGt` highRange voice ranges

-- | Get the bottom limit of the voice range
lowRange :: VoiceName -> VoiceRanges -> Pitch
lowRange voice ranges = fst $ ranges !! fromEnum voice

-- | Get the top limit of the voice range
highRange :: VoiceName -> VoiceRanges -> Pitch
highRange voice ranges = snd $ ranges !! fromEnum voice

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

-- | Go through list of pitches and reduce intervals that are more than a seventh
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

-- | Adjust a whole 'Voice' stepwise
stepwiseVoiceInRange :: Voice -> VoiceRanges -> Voice
stepwiseVoiceInRange v ranges = Voice {
    voiceID = voiceID v,
    music   = stepwiseInRange (music v) (voiceID v) ranges
}


-- | Go through list of pitches and reduce intervals that are more than a seventh
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

-- | Reduce leap of more than a seventh by shifting octave of second note up
-- or down until the interval is within range
--
-- __TODO__ : 
--    - but what if after adjusting for leaps, a note is out of range?
--    - and what if there is a descending scale that goes out of range and the
--    only way to adjust it is to make a seventh? need to adjust a whole
--    phrase
unleap :: Pitch -> Pitch -> Pitch
unleap p1 p2
    | p7diff p1 p2 >= 6
        = unleap p1 $ octaveUp p2
    | p7diff p1 p2 <= -6
        = unleap p1 $ octaveDown p2
    | otherwise
        = p2

-- | Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
as !!? i | i >= length as = Nothing
        | otherwise      = Just $ as !! i

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

-- | Adjust a whole 'Voice' to be in range: check the highest and lowest notes
-- in the list, compare to the range for the voice, and shift the whole thing
-- by octave until all are in range; return error if it can't be done
voiceInRange :: Voice -> VoiceRanges -> Voice
voiceInRange voice ranges 
    | all (\p -> isPitchInRange p voiceName ranges) notes
        = voice
    | (tooLow && tooHighAfterAdjust) || (tooHigh && tooLowAfterAdjust)
        = voice
    | tooLow
        = voiceInRange (Voice voiceName $ map octaveUp notes) ranges
    | tooHigh
        = voiceInRange (Voice voiceName $ map octaveDown notes) ranges
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

        
    
-- * All together: From input parameters to music

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
        -> Int          -- ^ line count
        -> VoiceName    -- ^ voice name enum
        -> Perm         -- ^ contains random index for voice and rhythm
                        --      permutation
        -> Voice
ark2voice arca config penult sylCount lineCount voice perm =
    Voice { 
        voiceID = voice, 
        music   = newMusic 
    }
    where
        newMusic    = map (\ p -> pair2Pitch p voice vocalRanges mode modeList) pairs
        vocalRanges = ranges arca
        modeList    = modes arca
        mode        = arkMode config
        pairs       = zipFill rperm vpermVoice isRest $ fromEnum Rest

        style       = arkStyle config
        -- In syntagma 1 there is only one rperm for all four vperm voices;
        -- in syntagma 2 we match the four rperms to the four vperm voices.
        rperm       = case style of
                         Simple -> rpermChoir ! 0
                         Florid -> rpermChoir ! (fromEnum voice)
        
        vpermVoice  = getVoice arca newConfig sylCount lineCount voice vpermNum
        rpermChoir  = getRperm arca newConfig sylCount lineCount rpermNum

        newConfig   = ArkConfig {
            arkStyle        = style,
            arkMode         = arkMode config,
            arkMusicMeter   = arkMusicMeter config,
            arkTextMeter    = newTextMeter
        }
        oldTextMeter = arkTextMeter config
        newTextMeter | oldTextMeter == Prose  = proseMeter penult 
                     | otherwise              = oldTextMeter
        
        vpermNum    = voiceIndex perm
        rpermNum    = case style of
                         Simple -> rhythmIndex perm
                         Florid -> vpermNum

-- | Get music data for all four voices and pack them into a 'Chorus'.
--
-- __TODO__: Why do we use a @Fortuna@ 'Perm' here but just two integers in
-- the preceding function? Do we need to have all these parameters for both of
-- these functions?
--
-- We should be getting a new 'Perm' for each Chorus.
getChorus :: Arca       -- ^ ark data structure
        -> ArkConfig    -- ^ we pass this along to 'ark2voice'
        -> LyricPhrase       -- ^ input text processed by @Lectio@
        -> Perm         -- ^ 'Perm' (from @Fortuna@, includes index for voice
                        --       and rhythm)
        -> Chorus
getChorus arca config phrase perm = voices
    where
--        voicesInRange      = map (\v -> voiceInRange v range) voices
--        voicesStepwise     = map stepwiseVoice voicesInitialRange
 --       voicesInitialRange = map (\v -> setVoiceInitialRange v range) voices
        voices             = map (\v -> ark2voice arca config penult sylCount lineCount v perm) 
                                [Soprano, Alto, Tenor, Bass]

        range       = ranges arca
        penult      = phrasePenultLength phrase
        sylCount    = phraseSylCount phrase
        lineCount   = phrasePosition phrase

-- | Set the starting note of a voice to be within the proper range
setVoiceInitialRange :: Voice -> VoiceRanges -> Voice
setVoiceInitialRange voice ranges = Voice {
    voiceID = voiceID voice,
    music   = adjustFirstPitch $ music voice
}
    where
        adjustFirstPitch :: [Pitch] -> [Pitch]
        adjustFirstPitch (p:ps) 
            | isPitchRest p
                = (p:(adjustFirstPitch ps)) 
            | otherwise 
                = ((adjustPitchInRange p (voiceID voice) ranges):ps)

-- | A 'Symphonia' is the amalgamation of a list of 'Chorus'es into one
-- 'Chorus'
data Symphonia = Symphonia {
    chorus  :: Chorus, 
    lyricSection :: LyricSection
}

-- | Turn @[[S, A, T, B], [S1, A1, T1, B1]]@ into 
-- @[[S, S1], [A, A1], [T, T1], [B, B1]]@
mergeChoruses :: [Chorus] -> Chorus
mergeChoruses cs = map ( \vs -> mergeVoices vs) $ transpose cs

-- | To make a 'Symphonia' we take a 'LyricSentence' and list of 'Perm's, use
-- 'getChorus' to get the ark data for each 'LyricPhrase' in the sentence, each
-- using its own 'Perm'; then we use 'transpose' to reorder the lists. 
--
-- Where we had @[[S1, A1, T1, B1], [S2, A2, T2, B2], [S3, A3, T3, B3]]@
-- we end with @[[S1, S2, S3], [A1, A2, A3], [T1, T2, T3], [B1, B2, B3]]@.
-- A list of four voices becomes four lists of voices.
-- We need to combine each of those voices into a single voice to have a list
-- of four (longer) voices.
--
-- Adjust music of merged voices to avoid bad intervals ('stepwise').
--
-- The @Scribo@ module calls this function to get all the ark data needed to
-- set a whole 'LyricSentence', in the central function of our implementation,
-- @Scribo.compose@.
getSymphonia :: Arca -> LyricSection -> SectionPerm -> Symphonia
getSymphonia arca section sectionPerms = Symphonia {
        chorus = mergeChoruses subSymphoniae, 
        lyricSection = section
    }
    where
        subSymphoniae = map (\ (s,p) -> innerGetSymphonia arca config s p) 
                            $ zip (sentences section) sectionPerms
        config = sectionConfig section

        innerGetSymphonia :: Arca -> ArkConfig -> LyricSentence -> SentencePerm -> Chorus
        innerGetSymphonia arca config sentence perms = symphonia
            where
                symphonia   = map (\v -> stepwiseVoiceInRange v $ ranges arca) merged 
                merged      = mergeChoruses choruses 
                choruses    = map (\(p,s) -> getChorus arca config p s) permPhrases
                permPhrases = zip (phrases sentence) perms
    
-- | Get all the music for the sections from input
getMasterMusic :: Arca -> [LyricSection] -> [SectionPerm] -> [Symphonia]
getMasterMusic arca sections perms = 
    map (\ (s,p) -> getSymphonia arca s p) $ zip sections perms


--------------------------------------------------------------------------------
-- 2021/06/17

-- | A note ready to be written out: contains a 'Pitch' and a 'Syllable'
-- (text).
data Note = Note {
    pitch :: Pitch,
    lyric :: Syllable
}

-- | A single syllable to be paired with a 'Pitch', including its position in
-- the word.
data Syllable = Syllable {
    sylText :: String,
    sylPosition :: SyllablePosition
}

-- | What is the position of the syllable relative to the word? Beginning,
-- middle, or end? This determines hyphenation.
data SyllablePosition =   First 
                        | Middle
                        | Last

---- | One phrase of text paired with the notes for a single voice (e.g.,
---- Soprano).
--type Voice = [Note]
--
---- | A four-part combination of voices, SATB, such as for a single phrase of text
--data Chorus = Chorus {
--    soprano :: Voice,
--    alto    :: Voice,
--    tenor   :: Voice,
--    bass    :: Voice
--}
--
---- | Access one voice from a chorus by 'VoiceName'
--voiceFromChorus :: Chorus -> VoiceName -> Voice
--voiceFromChorus chorus voiceID = member chorus
--    where member = [soprano, alto, tenor, bass] !! fromEnum voiceID
--
---- | A list of 4-part musical settings, as for phrases in a section
--type Symphonia = [Chorus]
--
---- | Extract all the music for a particular 'VoiceName' from a 'Symphonia' as a single 'Voice'
--voiceFromSymphonia :: Symphonia -> VoiceName -> Voice
--voiceFromSymphonia symphonia voiceID = concat 
--    $ map (\ch -> voiceFromChorus ch voiceID) symphonia
--
---- | Take a 'Symphonia', this is, a list of 'Chorus' items, and combine the
---- data for each voice so that we have a single 'Chorus'
---- Similar to converting @[[S1, A1, T1, B1], [S2, A2, T2, B2]]@ 
---- to @[[S1, S2], [A1, A2], [T1, T2], [B1, B2]]@
--symphonia2chorus :: Symphonia -> Chorus
--symphonia2chorus symphonia = Chorus {
--    soprano = voiceFromSymphonia symphonia Soprano,
--    alto    = voiceFromSymphonia symphonia Alto,
--    tenor   = voiceFromSymphonia symphonia Tenor,
--    bass    = voiceFromSymphonia symphonia Bass
--}
--
--lyricText
--    lyricSection
--        lyricSentence (=stanza)
--            lyricPhrase
--
--    vperms
--    rperms

type MusicScore     = [MusicSection]
type MusicSection   = [MusicSentence]
type MusicSentence  = [MusicPhrase]
type MusicPhrase    = [Note]
data Note = Note { 
    notePitch :: Pitch, 
    noteLyric :: Syllable
}

-- | potential replacement for 'getMasterMusic' above
makeMusicScore :: Arca
                    -> [LyricSection]
                    -> [SectionPerm]
                    -> MusicScore
makeMusicScore arca lyricSections sectionPerms =
    map (\(sec, perm) -> 
        map (\voice -> makeMusicSection sec voice perm) 
            [Soprano, Alto, Tenor, Bass])
        $ zip lyricSections sectionPerms

makeMusicSection :: Arca 
                    -> LyricSection 
                    -> VoiceName
                    -> SectionPerm 
                    -> MusicSection
makeMusicSection arca section voiceID sectionPerms = musicSection
    where
-- for a single VOICE!
--    extract ArkConfig for whole section
--
--    for each sentence in section:
--      extract list of perms, one per phrase
--      extract list of lyric phrases
--      apply same ArkConfig
--
--      for each phrase in sentence:
--          look up vperm according to config and perm
--              (for some pinakes, choose column by stanza = section num)
--          look up rperm according to config and perm
--              (for syntagma II, use same perm)
--      
--          convert vperm nums to pitch names
--          (adjust pitches)
--          make Pitches: match pitches and rhythms, accounting for rests
--          
--          match Notes: match each Pitch with Phrase/Verbum/Syllable
--              according to syntagma
--          
--          return a MusicPhrase
--      inside a MusicSentence
--  inside a MusicSection

    musicSection = map (\(sentence, perm) 
            -> makeMusicSentence arca config sentence voiceID perm) 
        $ zip sentences sectionPerms

    config = sectionConfig section
    sentences = sentences section

makeMusicSentence :: Arca 
                    -> ArkConfig 
                    -> LyricSentence 
                    -> VoiceName
                    -> SectionPerm 
                    -> MusicSentence
makeMusicSentence arca config sentence voiceID sentencePerms = 
    map (\(phrase, perm) -> makeMusicPhrase arca config phrase voiceID perm) 
        $ zip (phrases sentence) sentencePerms

makeMusicPhrase :: Arca 
                    -> ArkConfig 
                    -> LyricPhrase 
                    -> VoiceName
                    -> Perm 
                    -> MusicPhrase
makeMusicPhrase arca config phrase voiceID perm =
    map (\(music, syllable) -> Note { 
            notePitch = music,
            noteLyric = syllable
        }) $ zip music syllables
    where
        music = ark2voice arca config penult sylCount lineCount voiceID perm
    
        range       = ranges arca
        penult      = phrasePenultLength phrase
        sylCount    = phraseSylCount phrase
        lineCount   = phrasePosition phrase

        words = phraseText phrase
        syllables = map makeSyllables words


makeSyllables :: Verbum -> [Syllable]
makeSyllables word = map (\(i, syl) -> Syllable {
        sylText = syl,
        sylPosition = position i
    }) $ indexed $ verbumSyl word
    where
        position :: Int -> SyllablePosition
        sylPosition | i == 0                = First
                    | i == length verbumSyl = Last
                    | otherwise             = Middle

                

