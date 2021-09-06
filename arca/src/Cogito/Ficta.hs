{-|
Module      : Cogito.Ficta
Description : Adjust ark output for /musica ficta/
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module provides functions to adjust music created by the ark to follow conventions of /musica ficta/.
-}

module Cogito.Ficta where

import Debug.Trace
    (trace)

import Data.List
    (findIndex)

import Data.List.Index as I
    ( imap
    , indexed
    )

import Data.Function
    (on)

import Data.Maybe
    (fromJust)

import Aedifico
    ( Arca      (..)
    , ArkConfig (..)
    , Accid     (..)
    , AccidType (..)
    , Dur       (..)
    , Mode
    , ModeList
    , ModeSystem
    , Pitch     (..)
    , Pnum      (..)
    , System    (..)
    )

import Cogito.Musarithmetic
    ( MusicChorus  (..)
    , MusicSection (..) 
    , MusicPhrase  (..)
    , Note         (..)
    , modeMollis
    , modalFinal
    , p7diffMod
    , p12diffMod
    )

-- * Adjust accidentals of individual pitches

-- | Copy a 'Note' but change the 'Pitch'
changeNotePitch :: Note -> Pitch -> Note
changeNotePitch note pitch = Note {
    noteSyllable = noteSyllable note,
    notePitch = pitch
}

-- | Copy a 'Note' but adjust just its 'Pitch' according to a function.
adjustNotePitch :: (Pitch -> Pitch) -> Note -> Note
adjustNotePitch fn note = changeNotePitch note $ fn $ notePitch note


-- | Adjust the accidental either toward flats or toward sharps, within the
-- 'Accid' enum. If the accidental is unset we just return the original pitch.
-- We are treating all accidental shifts as /musica ficta/ and giving a
-- 'Suggested' 'accidType'.
accidentalShift :: Pitch 
                -> Accid
                -> Pitch
accidentalShift pitch direction
    | accid pitch == AccidNil 
        = pitch
    | newAccidNum < fromEnum Fl || newAccidNum > fromEnum Sh
        -- if you can't adjust any further, just return pitch unchanged.
        = pitch -- error "Cannot adjust accidental further"
    | otherwise = Pitch { 
        pnum      = pnum pitch, 
        oct       = oct pitch, 
        dur       = dur pitch, 
        accid     = toEnum newAccidNum,
        accidType = Suggested
    }
    where
        newAccidNum = operation (fromEnum $ accid pitch) 1
        operation = case direction of
            Fl -> (-)
            Sh -> (+)

-- | Lower a pitch a semitone
flatten :: Pitch -> Pitch
flatten pitch = accidentalShift pitch Fl

-- | Raise a pitch a semitone
sharpen :: Pitch -> Pitch
sharpen pitch = accidentalShift pitch Sh

-- | Copy pitch but change 'accid' and 'accidType'
changeAccid :: Accid -> AccidType -> Pitch -> Pitch
changeAccid newAccid newAccidType p
    | accid p == AccidNil = p
    | otherwise = Pitch {
        pnum       = pnum p,
        oct        = oct p,
        dur        = dur p,
        accid      = newAccid,
        accidType  = newAccidType
    }

changeNoteAccid :: Accid -> AccidType -> Note -> Note
changeNoteAccid newAccid newAccidType n = Note {
    noteSyllable = noteSyllable n,
    notePitch    = changeAccid newAccid newAccidType $ notePitch n
}

-- | Cancel an accidental (suggested)
cancel :: Pitch -> Pitch
cancel = changeAccid Na Suggested

-- | Cancel the 'Pitch' within a 'Note'.
noteCancel :: Note -> Note
noteCancel = adjustNotePitch cancel

-- | Make 'accidType' 'Suggested'
fictaAccid :: Pitch -> Pitch
fictaAccid p = changeAccid (accid p) Suggested p

-- | Make 'accidType' 'Written'
writeAccid :: Pitch -> Pitch
writeAccid p = changeAccid (accid p) Written p


-- * Utilities to adjust pitches by @MusicSection@

-- | Do something to the 'Note's in every 'MusicPhrase' within a 'MusicSection'
adjustNotesInSection :: ([Note] -> [Note]) -> MusicSection -> MusicSection
adjustNotesInSection fn sec = MusicSection {
    secVoiceID = secVoiceID sec,
    secConfig  = secConfig sec,
    secSentences = map (map (adjustNotesInPhrase fn)) $ secSentences sec
}

-- | Copy a 'MusicPhrase' but with new notes
changeNotesInPhrase :: MusicPhrase -> [Note] -> MusicPhrase
changeNotesInPhrase phrase newNotes = MusicPhrase {
    phraseVoiceID = phraseVoiceID phrase,
    notes         = newNotes
}

-- | Do something to the 'Note' list in a 'MusicPhrase'
adjustNotesInPhrase :: ([Note] -> [Note]) -> MusicPhrase -> MusicPhrase
adjustNotesInPhrase fn phrase = changeNotesInPhrase phrase $ fn $ notes phrase

-- | Fold a ficta-adjusting function over a 'MusicSection'
fixFictaInSection :: ([Note] -> Note -> [Note]) 
                        -- ^ fold function (arguments: stack and new item)
                  -> MusicSection 
                  -> MusicSection
fixFictaInSection fn = adjustNotesInSection $ foldStack fn 

-- | Fold a ficta-adjusting function over a 'MusicPhrase'
fixFictaInPhrase :: ([Note] -> Note -> [Note]) 
                        -- ^ fold function (arguments: stack and new item)
                  -> MusicPhrase
                  -> MusicPhrase
fixFictaInPhrase fn = adjustNotesInPhrase $ foldStack fn

-- | Generate a stack folding function to process a list 
foldStack :: ([a] -> a -> [a]) -- ^ fold function (arguments: stack and new item)
          -> ([a] -> [a])
foldStack fn = reverse . foldl fn []


-- | Compare two 'MusicPhrase's and find the note in the one (lower) voice that
-- coincides rhythmically with a given note in the other (upper) voice. Used
-- to find harmonies and test them for bad intervals.
--
-- We find the top note by index and add the durations up to that point, then
-- we add the durations in the bottom voice up to each item (that is, a scan)
-- and then stop at the first item that matches the elapsed duration of the
-- top voice.
findCounterpoint :: MusicPhrase  -- ^ phrase to search for counterpoint
                 -> MusicPhrase   -- ^ phrase containing the point to match up 
                                  --     in the other voice
                 -> Int           -- ^ index of point in its phrase
                 -> Note
findCounterpoint counterpointPhrase pointPhrase index = counterpointNote
    where
        pointNotes          = notes pointPhrase
        counterpointNotes   = notes counterpointPhrase

        pointPitches        = map notePitch pointNotes
        counterpointPitches = map notePitch counterpointNotes

        pointLengths        = map (durQuantity . dur) pointPitches
        counterpointLengths = map (durQuantity . dur) counterpointPitches

        pointIndexElapsed   = sum $ take index pointLengths

        counterpointIndexSums  = scanl1 (+) counterpointLengths
        counterpointIndexMatch = 
            fromJust $ findIndex (> pointIndexElapsed) counterpointIndexSums

        counterpointNote       = counterpointNotes !! counterpointIndexMatch

-- | What is the elapsed time of a 'Dur' in units where 'Fs' (fusa) = 1?
durQuantity :: Dur -> Int
durQuantity dur | dur `elem` [Fs, FsR]    = 1
                | dur `elem` [Sm, SmR]    = 2
                | dur == SmD              = 3
                | dur `elem` [Mn, MnR]    = 4
                | dur == MnD              = 6
                | dur `elem` [Sb, SbR]    = 8
                | dur == SbD              = 12
                | dur `elem` [Br, BrR]    = 16
                | dur == BrD              = 24
                | dur `elem` [Lg, LgR]    = 32
                | dur == LgD              = 48
                | dur == DurNil           = error "can't compute this unset dur"
                | otherwise               = error $ "unknown dur " ++ show dur

-- * Apply /ficta/ adjustments to whole @MusicPhrase@s

-- | Adjust a chorus for /musica ficta/: adjust bass first, and then apply a
-- function to each of the upper voices adjusting it relative to the revised
-- bass.
adjustFictaChorus :: ModeSystem -> ModeList -> MusicChorus -> MusicChorus
adjustFictaChorus modeSystems modeList chorus = MusicChorus {
    soprano = adjustUpper $ soprano chorus,
    alto    = adjustUpper $ alto chorus,
    tenor   = adjustUpper $ tenor chorus,
    bass    = adjustBass
}
    where
        mode        = arkMode $ secConfig $ bass chorus
        adjustBass  = adjustBassFicta modeSystems modeList mode $ bass chorus
        adjustUpper = adjustPhrasesInSection (adjustFictaPhrase modeList mode) adjustBass 
              
-- | Map a function to the phrases in one section (upper voice) relative to
-- the phrases in another section (lower voice).
adjustPhrasesInSection :: (MusicPhrase -> MusicPhrase -> MusicPhrase)
                                       -- ^ phrase transform function
                       -> MusicSection -- ^ lower voice section
                       -> MusicSection -- ^ upper voice section
                       -> MusicSection
adjustPhrasesInSection fn lowerSection thisSection = MusicSection {
    secVoiceID   = secVoiceID thisSection,
    secConfig    = secConfig thisSection,
    secSentences = zipWith adjustSentence
                        (secSentences lowerSection)
                        (secSentences thisSection)
}
    where
        adjustSentence lowerSentence thisSentence = 
            zipWith fn lowerSentence thisSentence


-- | Apply /musica ficta/ adjustments to a whole 'MusicPhrase'. Some
-- adjustments only involve this phrase, while others must be relative to the
-- bass voice. The bass voice should be adjusted already before this function
-- is called.
--
-- Adjustments: (1) scale degree seven should be sharp if that is suggested in
-- the mode table only if it is going up to eight and if the bass is on scale
-- degree five; (2) scale degree six should be flat if that is suggested in
-- the mode table only if it is going down to five; (3) repeated notes should
-- match the last accidental in the series [as long as the harmonies with the
-- bass are okay? TODO]; (4) there should be no cross-relations or augmented
-- fifths.
--
-- TODO active development; not everything working
adjustFictaPhrase :: ModeList -> Mode -> MusicPhrase -> MusicPhrase -> MusicPhrase
adjustFictaPhrase modeList mode bassPhrase thisPhrase = adjusted
    where
--        adjusted      = (intervals . repeats . flats . leadingTones) thisPhrase
        adjusted      = (intervals . repeats . flats) thisPhrase
        intervals     = fixIntervalsInPhrase bassPhrase
        repeats       = fixFictaInPhrase fixAccidRepeat 
        flats         = fixFictaInPhrase fixFlatSharp 
        leadingTones  = fixLeadingTonesInPhrase modeList mode bassPhrase 

-- ** Specific adjustments by rule

-- | Return the 0-indexed scale degree of a given pitch in a given mode
-- (scale degree 0 is the modal final)
scaleDegree :: ModeList -> Mode -> Pitch -> Int
scaleDegree modeList mode pitch = p7diffMod pitch $ modalFinal modeList mode

-- | Does this 'Pitch' have the given accidental as 'Suggested'?
isFictaAccid :: Accid -> Pitch -> Bool
isFictaAccid a p = accid p == a && accidType p == Suggested

-- | Apply 'isFictaAccid' to a 'Note'
isFictaAccidNote :: Accid -> Note -> Bool
isFictaAccidNote a n = isFictaAccid a $ notePitch n

-- | Fix bad intervals against a lower voice in a 'MusicPhrase'
fixIntervalsInPhrase :: MusicPhrase -> MusicPhrase -> MusicPhrase
fixIntervalsInPhrase lowerPhrase thisPhrase = 
    adjustNotesInPhrase (imap (\i thisNote -> 
            fixIntervals (findCounterpoint lowerPhrase thisPhrase i) thisNote))
        thisPhrase

-- | No cross relations, augmented fifths, or tritones between upper and lower
-- note
fixIntervals :: Note -- ^ lower note
             -> Note -- ^ upper note
             -> Note -- ^ adjusted upper note
fixIntervals lowerNote thisNote = adjustNotePitch (adjust $ notePitch lowerNote) thisNote
    where
        adjust :: Pitch -> Pitch -> Pitch
        adjust lowerPitch thisPitch 
                | isCrossRelation lowerPitch thisPitch 
                        = trace "fixed cross relation"
                            $ changeAccid (accid lowerPitch) Suggested thisPitch
                | isAugFifth lowerPitch thisPitch 
                        = trace "canceled upper accid to fix augmented fifth" 
                            $ cancel thisPitch 
                | isTritone lowerPitch thisPitch
                    = fixTritone lowerPitch thisPitch
                | otherwise = thisPitch

        fixTritone lower upper
--                | accid upper == Na && accid lower == Fl
--                        = trace "flattened upper accid to fix tritone" 
--                            $ flatten upper
--                | accid upper == Na && accid lower /= Fl
--                        = trace "sharped upper accid to fix tritone" 
--                            $ sharpen upper
                | accid upper == Fl && accid lower == Na
--                    && pnum upper /= PCb
                        = trace "canceled upper non-B flat to fix tritone"
                            cancel upper
                | otherwise  = upper

-- | Avoid cross relations (TODO other intervals?) between upper voices.
-- If there is a cross relation on notes with 'Suggested' accidentals, match
-- the lower voice.
fixUpperVoiceIntervals lowerNote thisNote = 
    adjustNotePitch (adjust $ notePitch lowerNote) thisNote
    where
        adjust :: Pitch -> Pitch -> Pitch
        adjust lowerPitch thisPitch
            | isCrossRelation lowerPitch thisPitch
                && accidType thisPitch == Suggested 
                = changeAccid (accid lowerPitch) Suggested thisPitch
            | otherwise = thisPitch

-- | Are these pitches the same pitch class but different accidentals? (E.g.,
-- F vs F#?)
isCrossRelation:: Pitch -> Pitch -> Bool
isCrossRelation p1 p2 = pnum p1 == pnum p2 
                    && accid p1 /= accid p2 

-- | Are these pitches an augmented fifth apart?
isAugFifth :: Pitch -- ^ lower pitch
           -> Pitch -- ^ higher pitch
           -> Bool
isAugFifth p1 p2 = p7diffMod p2 p1 == 4
            && accid p1 == Na 
            && accid p2 == Sh 

-- | Are these two notes a tritone apart chromatically?
isTritone :: Pitch -- ^ lower pitch
          -> Pitch -- ^ upper pitch
          -> Bool
isTritone p1 p2 = p12diffMod p2 p1 == 6


-- | Adjust /musica ficta/ accidentals in the bass. 
adjustBassFicta :: ModeSystem -> ModeList -> Mode -> MusicSection -> MusicSection
adjustBassFicta modeSystems modeList mode bass = adjusted
    where
        adjusted = (repeats . sixSeven . tritones) bass
        repeats  = fixFictaInSection fixAccidRepeat 
        sixSeven = fixFictaInSection $ fixSixSeven modeList mode
        tritones = fixFictaInSection $ fixMelodicTritone modeSystems mode

-- | Fix melodic tritone before or after B (fold function)
fixMelodicTritone :: ModeSystem -> Mode -> [Note] -> Note -> [Note]
fixMelodicTritone modeSystems mode [] x = [x]
fixMelodicTritone modeSystems mode (x:xs) new
    | isMollis && isBflat x && isEnatural new
            = trace "made next note Eb to avoid bass tritone" 
                (flattenNote new):x:xs
    | isMollis && isEnatural x && isBflat new
            = trace "made previous note Eb to avoid bass tritone"
                new:(flattenNote x):xs 
    | (isTritone `on` notePitch) x new && isSuggested x
            = trace "canceled next non-B accidental to avoid bass tritone"
                new:(cancelNote x):xs
    | (isTritone `on` notePitch) x new && isSuggested new
            = trace "canceled previous non-B accidental to avoid bass tritone"
                (cancelNote new):x:xs
    | otherwise = new:x:xs
    where
        isMollis      = modeMollis mode modeSystems 
        isBflat       = testPitchAccid PCb Fl 
        isEnatural    = testPitchAccid PCe Na
        isSuggested n = accidType (notePitch n) == Suggested
        flattenNote   = adjustNotePitch flatten
        cancelNote    = changeNoteAccid Na Suggested

-- | Test the 'Pnum' and 'Accid' of a 'Note' against given values
testPitchAccid :: Pnum -> Accid -> Note -> Bool
testPitchAccid thisPnum thisAccid n = pnum p == thisPnum && accid p == thisAccid
    where p = notePitch n


-- | Fix descending scale-degree sevens and ascending sixes (fold function)
fixSixSeven :: ModeList -> Mode -> [Note] -> Note -> [Note]
fixSixSeven modeList mode [] x = [x]
fixSixSeven modeList mode (x:xs) new
    | degree x == 6 && isFictaAccidNote Sh x && degree new /= 0
        = trace "canceled descending bass #7"
            new:(cancelNote x):xs
    | degree x == 5 && isFictaAccidNote Fl x && degree new == 6
        = trace "canceled ascending bass b6"
            new:(cancelNote x):xs
    | otherwise = new:x:xs
    where
        degree       = (scaleDegree modeList mode) . notePitch

-- | Cancel the 'Pitch' in a 'Note'
cancelNote :: Note -> Note
cancelNote n = adjustNotePitch cancel n

-- | If this note is suggested flat, and the next note is suggested sharp,
-- make this note natural.
fixFlatSharp :: [Note] -> Note -> [Note]
fixFlatSharp [] x = [x]
fixFlatSharp (x:xs) new 
    | (isFictaAccidNote Fl x && isFictaAccidNote Sh new)
        || (isFictaAccidNote Sh x && isFictaAccidNote Fl new)
         = trace "fixed b-# sequence" 
            new:(cancelNote x):xs
    | otherwise = new:x:xs

-- | If there are two subsequent accidental inflections of a note (e.g., F
-- natural--F sharp or vice versa), make the first note match the second.
-- This deals with a byproduct of 'sharpLeadingTone' with repeated notes,
-- where that function would turn F--F--G into F--F#--G. This function would
-- make it F#--F#--G.
fixAccidRepeat :: [Note] -> Note -> [Note]
fixAccidRepeat [] x = [x]
fixAccidRepeat (x:xs) new
    | pnum p1 == pnum p2
        && oct p1 == oct p2
        = trace "fixed repeated note accid" 
            new:(adjustNotePitch (matchAccid p2) x):xs
    | otherwise = new:x:xs
    where 
        p1 = notePitch x
        p2 = notePitch new
        matchAccid new old = changeAccid (accid new) (accidType new) old

-- | Fix the leading tones of a 'MusicPhrase'.
fixLeadingTonesInPhrase :: ModeList 
                        -> Mode 
                        -> MusicPhrase  -- ^ bass voice
                        -> MusicPhrase  -- ^ upper voice to be adjusted
                        -> MusicPhrase  -- ^ adjusted upper voice
fixLeadingTonesInPhrase modeList mode bassPhrase thisPhrase =
    changeNotesInPhrase thisPhrase adjusted
    where
        adjusted      = map fst $ foldl (fixLeadingTonePairs modeList mode) [] counterpoints
        counterpoints = foldl (\acc (i, note) -> 
                                (note, findCounterpoint bassPhrase thisPhrase i):acc) [] 
                                $ I.indexed $ notes thisPhrase

-- | Fold function for adjusting the leading tones in the upper voice of a
-- list of (upper, lower) 'Note' pairs
fixLeadingTonePairs :: ModeList 
                    -> Mode
                    -> [(Note, Note)] -- ^ stack of pairs of upper and lower notes
                    -> (Note, Note)   -- ^ next upper voice note pair
                    -> [(Note, Note)] -- ^ pairs with adjusted upper notes
fixLeadingTonePairs modeList mode [] x = [x]
fixLeadingTonePairs modeList mode ((hi, lo):his) (newHi, newLo) = 
    (newHi, newLo):((sharpLeadingTone modeList mode lo hi newHi), lo):his

-- | Sharp highest scale degree in an upper voice (1) when it leads up to
-- scale-degree eight, and (2) when the bass note is on scale degree
-- five. (TODO or two?)
--
-- Remember, these are all 0-indexed numbers instead of the 1-indexed numbers
-- used in speech (degree 6 here is "scale degree 7" in speech).
sharpLeadingTone :: ModeList -- ^ list (table) of modes from the ark
                    -> Mode  -- ^ current mode
                    -> Note  -- ^ bass note
                    -> Note  -- ^ current note to be adjusted
                    -> Note  -- ^ next note
                    -> Note
sharpLeadingTone modeList mode bassNote thisTopNote nextTopNote = Note {
    notePitch = newTopPitch, 
    noteSyllable = noteSyllable thisTopNote 
}
    where 
        newTopPitch | isLeadingTone topPitch
                        = if degree nextTopPitch == 0 
                            && degree bassPitch == 4
                            then fictaAccid topPitch
                            else trace "fixed #7" $ flatten topPitch
                    | otherwise
                        = topPitch

        isLeadingTone :: Pitch -> Bool
        isLeadingTone pitch = degree pitch == 6 
                                && accid pitch == Sh
                                && accidType pitch == Suggested

        topPitch     = notePitch thisTopNote
        nextTopPitch = notePitch nextTopNote
        bassPitch    = notePitch bassNote
        degree       = scaleDegree modeList mode

