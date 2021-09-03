{- first approach to making voices move stepwise without bad leaps
 -}

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

unleapFold :: VoiceRange -- ^ @(Pitch, Pitch)@: bottom and top of range
           -> [Pitch]    -- ^ accumulator list (stack) 
           -> Pitch      -- ^ next pitch
           -> [Pitch]    -- ^ adjusted list/stack
unleapFold _ [] x = [x]
unleapFold range (x:xs) new 
    | p7diff new x > _maxLeap       = (octaveUp new):x:xs
    | p7diff new x < (0 - _maxLeap) = (octaveDown new):x:xs
    | otherwise                     = new:x:xs

-- conditions for keeping 'new' unchanged:
--      new <= top
--      new >= bottom
--      new - prev <= maxLeap
--      new - prev >= -maxLeap
--
--  new2 | new > top    = 8vb new
--       | new < bottom = 8va new
--       | otherwise    = new
--
--  new3 | new2 - prev > _maxLeap     = 8va new
--       | new2 - prev < 0 - _maxLeap = 8vb new
--       | otherwise                  = new2
--

stepwiseInRange :: VoiceRange -> [Pitch] -> [Pitch]
stepwiseInRange (low, high) ps = 

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
    | all (\p -> pitchInRange p voiceName ranges) notes
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



