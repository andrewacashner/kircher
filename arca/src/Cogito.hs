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

import Data.Vector 
    ((!))

import Aedifico 
    (Pnum        (..),
     Accid       (..),
     Octave      (OctNil),
     VoiceName   (..),
     VoiceRanges,
     Dur         (Br, BrR),
     Mode        (..),
     Style,
     TextMeter   (..),
     PenultLength (..),
     ArkConfig   (..),
     Arca        (..),
     System      (..),
     ModeSystem, 
     Pitch       (..),
     PnumAccid,
     ModeList,
     getVoice,
     getRperm,
     proseMeter)

import Fortuna 
    (Perm (voiceIndex, rhythmIndex),
     SectionPerm,
     SentencePerm)

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

-- | Is a mode in /cantus mollis/? Should there be a flat in the key
-- signature?
modeMollis :: Mode -> ModeSystem  -> Bool
modeMollis mode systems =
    let s = systems ! fromEnum mode
    in case s of
        Durus  -> False
        Mollis -> True

-- | Adjust a pitch to be in a given mode. 
pnumAccidInMode :: Pnum -> Mode -> ModeList -> PnumAccid
pnumAccidInMode pnum mode modeList = modeList ! fromEnum mode ! fromEnum pnum
   

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
absPitch p = oct12 + pnum12 + accid12
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

-- | Raise the octave by 1
octaveUp :: Pitch -> Pitch
octaveUp p = Pitch { 
        pnum  = pnum p,
        oct   = oct p + 1,
        accid = accid p,
        dur   = dur p
}

-- | Lower the octave by 1
octaveDown :: Pitch -> Pitch
octaveDown p = Pitch { 
        pnum  = pnum p,
        oct   = oct p - 1,
        accid = accid p,
        dur   = dur p
}
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

-- | Is the 'Pitch' a rest?
isPitchRest :: Pitch -> Bool
isPitchRest p = isRest $ dur p

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
-- ('pitchInRange').
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
        else pitchInRange pitch voice ranges
        where
            pitch = stdPitch RawPitch {
                rawPnum    = fromEnum $ fst modePitch,
                rawAccid   = snd modePitch,
                rawOct     = voice2octave voice,
                rawDur     = thisDur
            } 
            modePitch = pnumAccidInMode thisPnum mode modeList
            thisPnum  = toPnum $ snd pair
            thisDur   = fst pair


-- | Get the right starting octave range for each voice type
voice2octave :: VoiceName -> Int
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
pitchInRange :: Pitch -> VoiceName -> VoiceRanges -> Pitch
pitchInRange pitch voice ranges
    | pitchTooLow pitch voice ranges 
        = pitchInRange (octaveUp pitch) voice ranges
    | pitchTooHigh pitch voice ranges
        = pitchInRange (octaveDown pitch) voice ranges
    | otherwise = pitch


-- ** Adjust list of pitches to avoid bad intervals

-- | Go through list of pitches and reduce intervals that are more than a seventh
stepwise :: [Pitch] -> VoiceName -> VoiceRanges -> [Pitch]
stepwise [] _ _ = []
stepwise (a:[]) _ _ = [a]
stepwise (a:b:[]) _ _ = [a, (unleap a b)]
stepwise (a:b:c:cs) voice ranges =
    -- If b is a rest, calculate interval between a and b, adjust the next item
    -- c as though it was preceded by a
    if isPitchRest b
    then 
        let c2 = unleap a $ c
        in (a:b:(stepwise (c2:cs) voice ranges)) 
    else
        let b2 = unleap a b
        in
            -- If the adjusted second pitch b2 is out of range, then we need
            -- to adjust the octave of a accordingly and start over;
            -- If not, continue with a, b2, rest of list
            if pitchTooLow b2 voice ranges
            then stepwise ((octaveUp a):b:c:cs) voice ranges
            else if pitchTooHigh b2 voice ranges
            then stepwise ((octaveDown a):b:c:cs) voice ranges
            else a:(stepwise (b2:c:cs) voice ranges)

-- | Adjust a whole 'Voice' stepwise
stepwiseVoice :: Voice -> VoiceRanges -> Voice
stepwiseVoice v ranges = Voice {
    voiceID = voiceID v,
    music   = stepwise (music v) (voiceID v) ranges
}

-- | Reduce leap of more than a seventh by shifting octave of second note up
-- or down until the interval is within range
unleap :: Pitch -> Pitch -> Pitch
unleap p1 p2
    | p7diff p1 p2 >= 6
        = unleap p1 $ octaveUp p2
    | p7diff p1 p2 <= -6
        = unleap p1 $ octaveDown p2
    | otherwise
        = p2

-- __TODO__ : but what if after adjusting for leaps, a note is out of range?


    
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
        -> VoiceName    -- ^ voice name enum
        -> Perm         -- ^ contains random index for voice and rhythm
                        --      permutation
        -> Voice
ark2voice arca config penult sylCount voice perm =
    Voice { 
        voiceID = voice, 
        music   = newMusic 
    }
    where
        newMusic    = map (\ p -> pair2Pitch p voice vocalRanges mode modeList) pairs
        vocalRanges = ranges arca
        modeList    = modes arca
        mode        = arkMode config
        pairs       = zipFill rperm vpermVoice isRest (fromEnum Rest) 
        
        vpermVoice  = getVoice arca newConfig sylCount voice vpermNum
        rperm       = getRperm arca newConfig sylCount rpermNum

        newConfig   = ArkConfig {
            arkStyle        = arkStyle config,
            arkMode         = arkMode config,
            arkMusicMeter   = arkMusicMeter config,
            arkTextMeter    = newTextMeter
        }
        oldTextMeter = arkTextMeter config
        newTextMeter | oldTextMeter == Prose  = proseMeter penult 
                     | otherwise              = oldTextMeter
        
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
data Symphonia = Symphonia {
    chorus  :: Chorus, 
    section :: Section
}

-- | Turn @[[S, A, T, B], [S1, A1, T1, B1]]@ into 
-- @[[S, S1], [A, A1], [T, T1], [B, B1]]@
mergeChoruses :: [Chorus] -> Chorus
mergeChoruses cs = map ( \vs -> mergeVoices vs) $ transpose cs

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
-- Adjust music of merged voices to avoid bad intervals ('stepwise').
--
-- The @Scribo@ module calls this function to get all the ark data needed to
-- set a whole 'Sentence', in the central function of our implementation,
-- @Scribo.compose@.
getSymphonia :: Arca -> Section -> SectionPerm -> Symphonia
getSymphonia arca section sectionPerms = Symphonia {
        chorus = mergeChoruses subSymphoniae, 
        section = section
    }
    where
        subSymphoniae = map (\ (s,p) -> innerGetSymphonia arca config s p) 
                            $ zip (sentences section) sectionPerms
        config = sectionConfig section

        innerGetSymphonia :: Arca -> ArkConfig -> Sentence -> SentencePerm -> Chorus
        innerGetSymphonia arca config sentence perms = symphonia
            where
                symphonia   = map (\ vs -> stepwiseVoice vs vocalRanges) merged
                vocalRanges = ranges arca
                merged      = mergeChoruses choruses 
                choruses    = map (\ (p,s) -> getChorus arca config p s) permPhrases
                permPhrases = zip (phrases sentence) perms

-- | Get all the music for the sections from input
getMasterMusic :: Arca -> [Section] -> [SectionPerm] -> [Symphonia]
getMasterMusic arca sections perms = 
    map (\ (s,p) -> getSymphonia arca s p) $ zip sections perms

