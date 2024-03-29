{-|
Module      : Cogito.Ficta
Description : Adjust ark output for /musica ficta/
Copyright   : (c) 2022 Andrew A. Cashner
Stability   : Experimental

This module provides functions to adjust music created by the ark to follow conventions of /musica ficta/.
-}

module Cogito.Ficta where

import Debug.Trace
    (trace)

import Data.List
    ( findIndex
    , foldl'
    )

import Data.List.Index as I
    ( imap
    , indexed
    )

import Data.Function
    (on)

import Data.Maybe
    ( fromJust
    , isNothing
    , maybe
    )

import Aedifico
    ( Arca      (..)
    , ArkConfig (..)
    , Accid     (..)
    , AccidType (..)
    , Dur       (..)
    , Tone
    , ToneList
    , ToneSystem
    , Pitch     (..)
    , Pnum      (..)
    , System    (..)
    , VoiceName (..)
    )

import Cogito.Musarithmetic
    ( MusicChorus  (..)
    , MusicSection (..) 
    , MusicPhrase  (..)
    , Note         (..)
    , toneMollis
    , modalFinal
    , p7diffMod
    , p12diffMod
    , pnumAccidEq
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

-- | Change the 'Accid' of the 'Pitch' within a 'Note'
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
foldStack fn = reverse . foldl' fn []

-- | Map a function to the phrases in one section (upper voice) relative to
-- the phrases in another section (lower voice).
adjustPhrasesRelative :: (MusicPhrase -> MusicPhrase -> MusicPhrase)
                                       -- ^ phrase transform function
                       -> MusicSection -- ^ lower voice section
                       -> MusicSection -- ^ upper voice section
                       -> MusicSection
adjustPhrasesRelative fn lower upper = MusicSection {
    secVoiceID   = secVoiceID upper,
    secConfig    = secConfig upper,
    secSentences = zipWith (zipWith fn) (secSentences lower) (secSentences upper)
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
findCounterpoint cptPhrase ptPhrase index = 
    maybe (error "no counterpoint found") ((!!) cptNotes) cptIndexMatch
    where
        ptNotes    = notes ptPhrase
        cptNotes   = notes cptPhrase

        ptPitches  = map notePitch ptNotes
        cptPitches = map notePitch cptNotes

        ptLengths  = map (durQuantity . dur) ptPitches
        cptLengths = map (durQuantity . dur) cptPitches

        ptIndexElapsed  = sum $ take index ptLengths

        cptIndexSums    = scanl1 (+) cptLengths
        cptIndexMatch   = findIndex (> ptIndexElapsed) cptIndexSums

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

-- | Apply /ficta/ adjustments to whole @MusicPhrase@s. Adjust bass voice
-- first; then adjust the upper voices individually, then adjust them
-- again relative to the bass.
--
-- TODO nowhere do we deal with toneB (needed in s2/p4 for every 3rd and 4th
-- line)
adjustFictaChorus :: ToneSystem -> ToneList -> MusicChorus -> MusicChorus
adjustFictaChorus toneSystems toneList chorus = MusicChorus {
    cantus  = adjustCantus,
    alto    = adjustAlto,
    tenor   = adjustTenor,
    bass    = adjustBass
}
    where
        tone         = arkTone $ secConfig $ bass chorus
        adjust       = adjustFictaVoice toneList tone 
        adjustBass   = adjust $ bass chorus 
        adjustCantus = adjustRelBass toneList tone adjustBass 
                        $ adjust $ cantus chorus
        adjustAlto   = adjustRelUpper adjustCantus 
                        $ adjustRelBass toneList tone adjustBass 
                        $ adjust $ alto chorus
        adjustTenor  = adjustRelUpper adjustCantus 
                        $ adjustRelUpper adjustAlto
                        $ adjustRelBass toneList tone adjustBass 
                        $ adjust $ tenor chorus

-- | Adjust /musica ficta/ for all the notes for one voice in a
-- 'MusicSection.'
--
-- These rules are based on Kircher but had to be expanded as he doesn't
-- account for some important and common cases. 
--
-- TODO they still don't deal with every problem. Some would be automatically
-- corrected by competent performers.
--
-- fix "illicit intervals" in the bass (p. 71)
--
-- #^7: (p. 69-70)
-- - If the tone table has #^7, and next note is #8, keep the sharp
-- - If it has #^7 and it is the last note in the phrase, and it is not the
-- bass voice, keep the sharp
-- - Otherwise make it natural
--
-- b^6:
-- - If the tone table has b^6, cancel the flat if the next note is #^7
--
-- After making either of the above adjustments, go through and check for
-- repeated notes: make them match the next accidental (G♮-G# should be
-- G#-G#).
adjustFictaVoice :: ToneList -> Tone -> MusicSection -> MusicSection
adjustFictaVoice toneList tone sec = adjust sec
    where
        voiceID = secVoiceID sec
        adjust  = case voiceID of
            Bass -> repeats . sharpSevens . bassIntervals
            _    -> repeats . sharpSevens 
 
        repeats           = fixFictaInSection fixRepeats
        sharpSevens       = fixFictaInSection fixSharpSevens
        bassIntervals     = fixFictaInSection fixIllicitIntervals

        -- | Fix successive same pitches with different accidentals, make the
        -- first match the next (added to Kircher)
        -- TODO deal with three+ notes in a row
        fixRepeats :: [Note] -> Note -> [Note]
        fixRepeats [] next = [next]
        fixRepeats (x:xs) next 
            | pitchClass x /= pitchClass next = next:x:xs
            | (isFictaAccidNote Sh x || isFictaAccidNote Fl x)
                && isFictaAccidNote Na next 
                = trace "canceled (#/b)-♮"      next:(cancelNote x):xs
            | isNatural x && isFictaAccidNote Sh next
                = trace "raised ♮-#"            next:(sharpenNote x):xs
            | isNatural x && isFictaAccidNote Fl next
                = trace "lowered ♮-b"           next:(flattenNote x):xs
            | isFictaAccidNote Fl x && isNatural next
                = trace "lowered next in b-♮"   (flattenNote next):x:xs
            | otherwise = next:x:xs

        -- | If ^7 is sharp in the tone table, leave it if it ascends to ^8;
        -- otherwise natural. Keep flat sixes from tone table unless they move
        -- to #7 (added to Kircher's rules). 
        fixSharpSevens :: [Note] -> Note -> [Note]
        fixSharpSevens [] next = [next]
        fixSharpSevens (x:xs) next 
            | voiceID == Bass 
                && ((isSharpSeven x && isTritoneNote (cancelNote x) next)
                    || (isSharpSeven next && isTritoneNote (cancelNote next) x))
                = trace "leaving bass #7 to avoid tritone"
                    next:x:xs
            | voiceID == Bass && isSharpSeven x
                = trace "canceled this bass #7"
                    next:(cancelNote x):xs
            | voiceID == Bass && isSharpSeven next
                = trace "canceled next bass #7"
                    (cancelNote next):x:xs
            | isSharpSeven x && (not . isFinal) next  
                = trace "canceled non-ascending #7" 
                    next:(cancelNote x):xs
            | isFlatSix x && isSharpSeven next 
                = trace "canceled b6-#7" 
                    next:(cancelNote x):xs
            | (noteAccid x == Fl && noteAccid next == Sh)
                || (noteAccid x == Sh && noteAccid next == Fl)
                = trace "fixed b-# or #-b" 
                    next:(cancelNote x):xs
            | otherwise = next:x:xs


        -- | Avoid melodic tritones in the bass ONLY (Kircher p. 71)
        -- Cancel all sharp sevens in bass (?) unless to do so forms a melodic tritone
        fixIllicitIntervals [] next = [next]
        fixIllicitIntervals (x:xs) next
            | (isBnatural x || isBflat next) && tritoneNotes next x
                = trace "bad bass interval: flatten before" 
                    next:(flattenNote x):xs
            | isBflat x && tritoneNotes next x
                = trace "bad bass interval: flatten after" 
                    (flattenNote next):x:xs
            | isEflat x && (tritoneNotes next x || augFifthNotes next x)
                = trace "bad bass interval: cancel before" 
                    next:(cancelNote x):xs
            | isEflat next && tritoneNotes next x
                = trace "bad bass interval: cancel after" 
                    (cancelNote next):x:xs
            | isCsharp x && isThree x && tritoneNotes next x
                = trace "bass bass interval: cancel signature C#"
                    next:(cancelNote x):xs
            | isCsharp next && isThree next && tritoneNotes next x
                = trace "bass bass interval: cancel signature C#"
                    (cancelNote next):x:xs
            | otherwise = next:x:xs

        isSharpSeven n   = isSeven n && isFictaAccidNote Sh n 
        isNaturalSeven n = isSeven n && isFictaAccidNote Na n
        isNatural        = (== Na) . accid . notePitch 
        isFlatSix n      = isSix n && isFictaAccidNote Fl n
        isFinal          = (== 1) . degree
        isThree          = (== 3) . degree
        isSix            = (== 6) . degree
        isSeven          = (== 7) . degree
        degree           = (scaleDegree1 toneList tone) . notePitch

       
        isBflat          = checkPnumAccid PCb Fl
        isBnatural       = checkPnumAccid PCb Na
        isEflat          = checkPnumAccid PCe Fl
        isCsharp         = checkPnumAccid PCc Sh
        tritoneNotes     = isTritone `on` notePitch
        augFifthNotes    = isAugFifth `on` notePitch

        cancelNote       = changeNoteAccid Na Suggested
        sharpenNote      = changeNoteAccid Sh Suggested
        flattenNote      = changeNoteAccid Fl Suggested

-- | Pitch class of 'Note'
pitchClass :: Note -> Pnum
pitchClass = pnum . notePitch

-- | Accid of 'Note'
noteAccid :: Note -> Accid
noteAccid = accid . notePitch

-- | Test pitch class and accidental of 'Note'
checkPnumAccid :: Pnum -> Accid -> Note -> Bool
checkPnumAccid thisPnum thisAccid n = 
    pnum p == thisPnum && accid p == thisAccid
    where p = notePitch n


-- | Adjust /musica ficta/ in an upper voice relative to the bass. Avoid cross
-- relations, augmented fifths, and certain tritones. (TODO which tritones?)
adjustRelBass :: ToneList 
              -> Tone
              -> MusicSection -- ^ lower voice to compare
              -> MusicSection -- ^ upper voice to adjust
              -> MusicSection
adjustRelBass toneList tone = adjustPhrasesRelative (adjustFictaPhrase toneList tone) 
    where

        adjustFictaPhrase :: ToneList -> Tone -> MusicPhrase -> MusicPhrase -> MusicPhrase
        adjustFictaPhrase toneList tone lower = fixIntervalsInPhrase lower
        
        fixIntervalsInPhrase lowerPhrase thisPhrase = 
            adjustNotesInPhrase (imap (\i thisNote -> 
                    fixIntervals (findCounterpoint lowerPhrase thisPhrase i) thisNote))
                thisPhrase

        -- | No cross relations or augmented fifths between upper and lower note;
        -- tritones are okay if the bass is moving up by semitone (F/B), not if the
        -- bass is flat (E/Bb), not if the bass is ^2
        fixIntervals :: Note -- ^ lower note
                     -> Note -- ^ upper note
                     -> Note -- ^ adjusted upper note
        fixIntervals lowerNote thisNote = adjustNotePitch (adjust $ notePitch lowerNote) thisNote
            where
                adjust :: Pitch -> Pitch -> Pitch
                adjust lowerPitch thisPitch 
                        | isCrossRelation lowerPitch thisPitch 
                            = trace "fixed cross relation" 
                                changeAccid (accid lowerPitch) Suggested thisPitch
                        | isAugFifth lowerPitch thisPitch 
                            = trace "canceled upper accid to fix augmented fifth" 
                                cancel thisPitch 
                        | isTritone lowerPitch thisPitch 
                            = trace "found tritone against bass:" 
                                tritone lowerPitch thisPitch
                        | otherwise = thisPitch

                tritone lowerPitch thisPitch
                        | accid lowerPitch == Fl
                            = trace "flattened upper note against bass flat"
                                flatten thisPitch
                        | accid thisPitch == Fl
                            && scaleDegree1 toneList tone lowerPitch == 2
                            = trace "canceled upper b against bass ^2"
                                cancel thisPitch
                        | accid thisPitch == Fl
                            && isFictaAccid Na lowerPitch
                            = trace "canceled upper b against canceled bass"
                                cancel thisPitch
                        | accid lowerPitch == Sh
                            = trace "raised upper note against bass #"
                                sharpen thisPitch
                        | accid thisPitch == Na 
                            && pnum thisPitch == PCf 
                            && pnum lowerPitch == PCb
                            = trace "raised upper F against bass B"
                                sharpen thisPitch
                        | otherwise 
                            = trace "tritone is okay"
                                thisPitch


-- | Adjust the 'Note's in a 'MusicSection' relative to the voice above: avoid
-- cross-relations between upper voices; where voices disagree, favor the
-- upper voice.
adjustRelUpper :: MusicSection -- ^ upper voice to compare
               -> MusicSection -- ^ lower voice to adjust
               -> MusicSection
adjustRelUpper = adjustPhrasesRelative adjustFictaPhrase
    where 
        adjustFictaPhrase :: MusicPhrase -> MusicPhrase -> MusicPhrase
        adjustFictaPhrase upper = adjustFictaNotes upper

        adjustFictaNotes upper lower =
            adjustNotesInPhrase (imap (\i n -> 
                fixUpperCrossRelations (findCounterpoint upper lower i) n))
            lower
        
        fixUpperCrossRelations :: Note -- ^ upper note to compare
                               -> Note -- ^ lower note to adjust
                               -> Note -- ^ adjusted lower note
        fixUpperCrossRelations upper lower
            | (isCrossRelation `on` notePitch) upper lower
                = trace "fixed cross relation in upper voices"
                    changeNoteAccid (noteAccid upper) Suggested lower
            | otherwise = lower


-- ** Specific adjustments by rule

-- | Return the 0-indexed scale degree of a given pitch in a given tone
-- (scale degree 0 is the modal final)
scaleDegree :: ToneList -> Tone -> Pitch -> Int
scaleDegree toneList tone pitch = p7diffMod pitch $ modalFinal toneList tone

-- | 1-indexed scale degree (more intelligible to the musical programmer)
scaleDegree1 toneList tone pitch = scaleDegree toneList tone pitch + 1

-- | Does this 'Pitch' have the given accidental as 'Suggested'?
isFictaAccid :: Accid -> Pitch -> Bool
isFictaAccid a p = accid p == a && accidType p == Suggested

-- | Apply 'isFictaAccid' to a 'Note'
isFictaAccidNote :: Accid -> Note -> Bool
isFictaAccidNote a n = isFictaAccid a $ notePitch n

-- | Are these pitches the same pitch class but different accidentals? (E.g.,
-- F vs F#?)
isCrossRelation:: Pitch -> Pitch -> Bool
isCrossRelation p1 p2 = pnum p1 == pnum p2 
                    && accid p1 /= accid p2 

-- | Are these pitches an augmented fifth apart?
isAugFifth :: Pitch -- ^ lower pitch
           -> Pitch -- ^ higher pitch
           -> Bool
isAugFifth p1 p2 = p12diffMod p2 p1 == 8 && p7diffMod p2 p1 == 4

-- | Is this a tritone (diminished fifth or augmented fourth?)
isTritone :: Pitch -> Pitch -> Bool
isTritone p1 p2 = p12diffMod p2 p1 == 6 && (p7diffMod p2 p1 `elem` [3, 4])

-- | Is the interval between these 'Note's a tritone?
isTritoneNote = isTritone `on` notePitch
