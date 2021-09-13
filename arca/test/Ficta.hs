{- unneeded adjustment functions from the Cogito.Ficta module -}

-- | Are these two notes a tritone apart chromatically?
isTritone :: Pitch -- ^ lower pitch
          -> Pitch -- ^ upper pitch
          -> Bool
isTritone p1 p2 = p12diffMod p2 p1 == 6


-- | Adjust /musica ficta/ accidentals in the bass. 
adjustBassFicta :: ToneSystem -> ToneList -> Tone -> MusicSection -> MusicSection
adjustBassFicta toneSystems toneList tone bass = adjusted
    where
        adjusted = (repeats . sixSeven . tritones) bass
        repeats  = fixFictaInSection fixAccidRepeat 
        sixSeven = fixFictaInSection $ fixSixSeven toneList tone
        tritones = fixFictaInSection $ fixMelodicTritone toneSystems tone

-- | Fix melodic tritone before or after B (fold function)
fixMelodicTritone :: ToneSystem -> Tone -> [Note] -> Note -> [Note]
fixMelodicTritone toneSystems tone [] x = [x]
fixMelodicTritone toneSystems tone (x:xs) new
    | isMollis && isBflat x && isEnatural new
            = trace "made next note Eb to avoid bass tritone" 
                (flattenNote new):x:xs
    | isMollis && isEnatural x && isBflat new
            = trace "made previous note Eb to avoid bass tritone"
                new:(flattenNote x):xs 
    | testPitchAccid PCa Na x && testPitchAccid PCe Fl new
            = trace "flattened prev A to avoid bass tritone with Eb"
                new:(flattenNote x):xs
    | testPitchAccid PCe Fl x && testPitchAccid PCa Na new
            = trace "flattened next A to avoid bass tritone with Eb"
                (flattenNote new):x:xs
--    | (isTritone `on` notePitch) x new && isSuggested x
--            = trace "canceled next non-B accidental to avoid bass tritone"
--                new:(cancelNote x):xs
--    | (isTritone `on` notePitch) x new && isSuggested new
--            = trace "canceled previous non-B accidental to avoid bass tritone"
--                (cancelNote new):x:xs
    | otherwise = new:x:xs
    where
        isMollis      = toneMollis tone toneSystems 
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
fixSixSeven :: ToneList -> Tone -> [Note] -> Note -> [Note]
fixSixSeven toneList tone [] x = [x]
fixSixSeven toneList tone (x:xs) new
    | degree x == 6 && isFictaAccidNote Sh x && degree new /= 0
        = trace "canceled descending bass #7"
            new:(cancelNote x):xs
    | degree x == 5 && isFictaAccidNote Fl x && degree new == 6
        = trace "canceled ascending bass b6"
            new:(cancelNote x):xs
    | otherwise = new:x:xs
    where
        degree       = (scaleDegree toneList tone) . notePitch

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
fixLeadingTonesInPhrase :: ToneList 
                        -> Tone 
                        -> MusicPhrase  -- ^ bass voice
                        -> MusicPhrase  -- ^ upper voice to be adjusted
                        -> MusicPhrase  -- ^ adjusted upper voice
fixLeadingTonesInPhrase toneList tone bassPhrase thisPhrase =
    changeNotesInPhrase thisPhrase adjusted
    where
        adjusted      = map fst $ foldl' (fixLeadingTonePairs toneList tone) [] counterpoints
        counterpoints = foldl' (\acc (i, note) -> 
                                (note, findCounterpoint bassPhrase thisPhrase i):acc) [] 
                                $ I.indexed $ notes thisPhrase

-- | Fold function for adjusting the leading tones in the upper voice of a
-- list of (upper, lower) 'Note' pairs
fixLeadingTonePairs :: ToneList 
                    -> Tone
                    -> [(Note, Note)] -- ^ stack of pairs of upper and lower notes
                    -> (Note, Note)   -- ^ next upper voice note pair
                    -> [(Note, Note)] -- ^ pairs with adjusted upper notes
fixLeadingTonePairs toneList tone [] x = [x]
fixLeadingTonePairs toneList tone ((hi, lo):his) (newHi, newLo) = 
    (newHi, newLo):((sharpLeadingTone toneList tone lo hi newHi), lo):his

-- | Sharp highest scale degree in an upper voice (1) when it leads up to
-- scale-degree eight, and (2) when the bass note is on scale degree
-- five. (TODO or two?)
--
-- Remember, these are all 0-indexed numbers instead of the 1-indexed numbers
-- used in speech (degree 6 here is "scale degree 7" in speech).
sharpLeadingTone :: ToneList -- ^ list (table) of tones from the ark
                    -> Tone  -- ^ current tone
                    -> Note  -- ^ bass note
                    -> Note  -- ^ current note to be adjusted
                    -> Note  -- ^ next note
                    -> Note
sharpLeadingTone toneList tone bassNote thisTopNote nextTopNote = Note {
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
        degree       = scaleDegree toneList tone

