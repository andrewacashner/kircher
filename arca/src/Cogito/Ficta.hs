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
    , izipWith
    )

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

-- | Adjust the accidental either toward flats or toward sharps, within the
-- 'Accid' enum. If the accidental is unset we just return the original pitch.
-- We are treating all accidental shifts as /musica ficta/ and giving a
-- 'Suggested' 'accidType'.
accidentalShift :: Pitch 
                -> Accid
                -> Pitch
accidentalShift pitch direction =
    if accid pitch == AccidNil
    then pitch
    else if newAccidNum < fromEnum FlFl || newAccidNum > fromEnum ShSh
    then error "Cannot adjust accidental further"
    else Pitch { 
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

-- | Lower a pitch a semitone, but not past 'FlFl'
flatten :: Pitch -> Pitch
flatten pitch = accidentalShift pitch Fl

-- | Raise a pitch a semitone, but not past 'ShSh'
sharpen :: Pitch -> Pitch
sharpen pitch = accidentalShift pitch Sh

-- | Copy pitch but change 'accid' and 'accidType'
changeAccid :: Pitch -> Accid -> AccidType -> Pitch
changeAccid p newAccid newAccidType 
    | accid p == AccidNil = p
    | otherwise = Pitch {
        pnum       = pnum p,
        oct        = oct p,
        dur        = dur p,
        accid      = newAccid,
        accidType  = newAccidType
    }

-- | Cancel an accidental (suggested)
cancel :: Pitch -> Pitch
cancel pitch = changeAccid pitch Na Suggested

-- | Cancel the 'Pitch' within a 'Note'.
noteCancel :: Note -> Note
noteCancel note = adjustNotePitch cancel note

-- | Make 'accidType' 'Suggested'
fictaAccid :: Pitch -> Pitch
fictaAccid pitch = changeAccid pitch (accid pitch) Suggested

-- | Make 'accidType' 'Written'
writeAccid :: Pitch -> Pitch
writeAccid pitch = changeAccid pitch (accid pitch) Written


-- * Utilities to adjust pitches by 'MusicSection'

-- | Map a function across every two items in a list. For a function that
-- takes each item of a list and the next item and returns the first (probably
-- modified), apply it to each item. Append the last item unchanged.
--
-- Example: combine x y = x + y
--          ls = [1, 2, 3, 4]
--          mapTwo combine ls = [combine 1 2, combine 2 3, combine 3 4, 4]
--                            => [3, 5, 7, 4]
mapTwo :: (a -> a -> a) -> [a] -> [a]
mapTwo fn (x:xs) = (zipWith fn (x:xs) xs) ++ [last xs]

-- | Map a function across every two items in an indexed list. The function
-- should take the index and the two items as input and return the first of
-- the two items (modified) as output. Append the last item unchanged.
imapTwo :: (Int -> a -> a -> a) -> [a] -> [a]
imapTwo fn (x:xs) = (I.izipWith fn (x:xs) xs) ++ [last xs]

-- | Do something to the 'Note's in every 'MusicPhrase within a 'MusicSection'
adjustNotesInSection :: ([Note] -> [Note])
                       -> MusicSection 
                       -> MusicSection
adjustNotesInSection fn sec = MusicSection {
    secVoiceID = secVoiceID sec,
    secConfig  = secConfig sec,
    secSentences = map (map (\phrase -> MusicPhrase {
        phraseVoiceID = phraseVoiceID phrase,
        notes         = fn $ notes phrase
    })) $ secSentences sec
}

-- | Apply a pitch-transformation function to every pitch in a 'MusicSection'
mapPitchesInSection :: (Pitch -> Pitch) -> MusicSection -> MusicSection
mapPitchesInSection fn = adjustNotesInSection (map (adjustNotePitch fn))

-- | Apply a pitch-transformation function to every pair of pitches in each
-- 'MusicPhrase' of a 'MusicSection'
mapPitchPairsInSection :: (Pitch -> Pitch -> Pitch) -- ^ pitch-transform function
                       -> MusicSection 
                       -> MusicSection
mapPitchPairsInSection fn = adjustNotesInSection (mapTwo (adjustNotePitchPairs fn))

-- | Apply a pitch-transformation function to every pair of pitches in each
-- 'MusicPhrase' of a 'MusicSection', but starting from end and moving to
-- front: in other words, map in reverse but return the list in its original
-- order (modified)
mapPitchPairsInSectionReverse :: (Pitch -> Pitch -> Pitch) -- ^ pitch-transform function
                       -> MusicSection 
                       -> MusicSection
mapPitchPairsInSectionReverse fn = 
    adjustNotesInSection (reverse . mapTwo (adjustNotePitchPairs fn) . reverse)


-- | Copy a 'Note' but adjust just its 'Pitch' according to a function.
adjustNotePitch :: (Pitch -> Pitch) -> Note -> Note
adjustNotePitch fn note = Note {
    noteSyllable = noteSyllable note,
    notePitch    = fn $ notePitch note
}

-- | Apply a function to groups of two 'Note's (not technically pairs), return
-- a copy of the first note with 'Pitch' modified by the function.
adjustNotePitchPairs :: (Pitch -> Pitch -> Pitch) -> Note -> Note -> Note
adjustNotePitchPairs fn note1 note2 = Note {
    noteSyllable = noteSyllable note1, 
    notePitch = fn (notePitch note1) (notePitch note2)
}

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

-- * Apply /ficta/ adjustments to whole 'MusicPhrase's

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
        adjustBass  = adjustBassFicta modeSystems modeList mode $ 
                        adjustTritoneAfterBflat modeSystems modeList mode 
                            $ bass chorus
        adjustUpper = adjustUpperRelBass
        adjustUpperRelBass = 
            adjustPhrasesInSection (adjustFictaPhrase modeList mode) adjustBass 
              
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
        adjustSentence lowerSentence thisSentence = zipWith fn lowerSentence thisSentence


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
adjustFictaPhrase modeList mode bassPhrase thisPhrase = MusicPhrase {
    phraseVoiceID = phraseVoiceID thisPhrase,
    notes = adjustPhrase
}
    where
        adjustPhrase  = (intervals . repeatedNotes . flats . leadingTones)
                            $ notes thisPhrase

        intervals     = imap (\i thisNote -> fixIntervals 
                            (findCounterpoint bassPhrase thisPhrase i) thisNote) 

        repeatedNotes = mapTwo matchRepeatedAccid 

        flats         = mapTwo adjustFlatSharpSequence 

        leadingTones  = imapTwo (\i this next -> (sharpLeadingTone modeList mode) 
                                    (findCounterpoint bassPhrase thisPhrase i) this next)

-- ** Specific adjustments by rule

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
-- TODO
--  problems: 
--      - repeated notes are rarely correct
--      - tritones against the bass
--      - in florid mode, there is not always a bass note to compare
--      - to make explicit or not?

-- | Return the 0-indexed scale degree of a given pitch in a given mode
-- (scale degree 0 is the modal final)
scaleDegree :: ModeList -> Mode -> Pitch -> Int
scaleDegree modeList mode pitch = p7diffMod pitch $ modalFinal modeList mode

-- | If there are two subsequent accidental inflections of a note (e.g., F
-- natural--F sharp or vice versa), make the first note match the second.
-- This deals with a byproduct of 'sharpLeadingTone' with repeated notes,
-- where that function would turn F--F--G into F--F#--G. This function would
-- make it F#--F#--G.
matchRepeatedAccid :: Note -- ^ note to adjust
                   -> Note -- ^ following note in sequence
                   -> Note
matchRepeatedAccid thisNote nextNote = Note {
    notePitch = adjustPitch,
    noteSyllable = noteSyllable thisNote
}
    where
        adjustPitch | pnum thisPitch == pnum nextPitch
                        && oct thisPitch == oct nextPitch
                        && accid thisPitch /= accid nextPitch
                        = trace "fixed repeated note accid" $
                            changeAccid thisPitch (accid nextPitch) (accidType nextPitch)
                     | otherwise = thisPitch
        
        thisPitch = notePitch thisNote 
        nextPitch = notePitch nextNote

-- | Copy a 'Note' but change the 'Pitch'
changeNotePitch :: Note -> Pitch -> Note
changeNotePitch note pitch = Note {
    noteSyllable = noteSyllable note,
    notePitch = pitch
}

-- | If this note is suggested flat, and the next note is suggested sharp,
-- make this note natural.
adjustFlatSharpSequence :: Note -> Note -> Note
adjustFlatSharpSequence n1 n2 = changeNotePitch n1 p1new
    where
        p1 = notePitch n1
        p2 = notePitch n2

        p1new | (isFictaAccid Fl p1 && isFictaAccid Sh p2)
                || (isFictaAccid Sh p1 && isFictaAccid Fl p2)
                    = trace "fixed b-# sequence" $ cancel p1
              | otherwise = p1

-- | Does this 'Pitch' have the given accidental as 'Suggested'?
isFictaAccid :: Accid -> Pitch -> Bool
isFictaAccid acc p = accid p == acc && accidType p == Suggested


-- | No cross relations, augmented fifths, or tritones
fixIntervals lowerNote thisNote = 
    adjustNotePitch (adjust $ notePitch lowerNote) thisNote
    where
        adjust :: Pitch -> Pitch -> Pitch
        adjust lowerPitch thisPitch 
                | isCrossRelation lowerPitch thisPitch 
                        = trace "fixed cross relation"
                            $ changeAccid thisPitch (accid lowerPitch) Suggested
                | isAugFifth lowerPitch thisPitch 
                        = trace "fixed augmented fifth" 
                            $ cancel thisPitch 
                | isTritone lowerPitch thisPitch 
                        = trace "fixed tritone" 
                            $ cancel thisPitch
                | otherwise  = thisPitch

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
                = changeAccid thisPitch (accid lowerPitch) Suggested
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

-- | Adjust tritones after a B flat: (implemented by going through notes in
-- reverse to find tritone /before/ the B flat, probably not the best way)
adjustTritoneAfterBflat :: ModeSystem -> ModeList -> Mode -> MusicSection -> MusicSection
adjustTritoneAfterBflat modeSystems modeList mode section = 
    mapPitchPairsInSectionReverse adjust section
    where
        adjust :: Pitch -> Pitch -> Pitch
        adjust p1 p2 | isTritone p1 p2 && pnum p2 == PCb && modeMollis mode modeSystems
                        = trace "flattened bass to avoid tritone" $ flatten p1
                     | otherwise = p1

-- | Adjust /musica ficta/ accidentals in the bass. Rules: 
--    - #7 => n7 (OR @#7 8  => unchanged@, @#7 _  => n7 _@)
--    - @b6 #7 => n6 #7@
--    - @b6 _  => unchanged@
adjustBassFicta :: ModeSystem -> ModeList -> Mode -> MusicSection -> MusicSection
adjustBassFicta modeSystems modeList mode bass = mapPitchPairsInSection adjust bass
    where
        adjust :: Pitch -> Pitch -> Pitch
        adjust p1 p2 | cancelSeven p1 p2 
                        = trace "canceled bass #7" $ cancel p1
                     | cancelSixth p1 p2
                        = trace "canceled bass b6" $ cancel p1
                     | isTritone p1 p2 && pnum p2 == PCb && modeMollis mode modeSystems
                        = trace "flattened bass to avoid tritone" $ flatten p1
                     | addBonusFlat p1 p2
                        = trace "added bonus flat to bass to avoid tritone" $ flatten p1
                     | delBonusFlat p1 p2
                        = trace "removed flat from bass to avoid tritone" $ cancel p1
                     | otherwise = p1

        cancelSeven p1 p2 = degree p1 == 6
                            && accid p1 == Sh 
                            && accidType p1 == Suggested 
                            && degree p2 /= 0 

        cancelSixth p1 p2 = degree p1 == 5
                            && accid p1 == Fl
                            && accidType p1 == Suggested
                            && degree p2 == 6
       
        addBonusFlat p1 p2 = degree p2 == 5
                            && isTritone p1 p2
                            && accid p2 == Fl
                            && accidType p2 == Suggested

        delBonusFlat p1 p2 = degree p2 == 5
                            && isTritone p1 p2
                            && accid p1 == Fl
                            && accid p2 == Na
                            && accidType p1 == Suggested
                        
        degree = scaleDegree modeList mode



