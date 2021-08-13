----------------------------------------------------------------------------
-- DEPRECATED
-- Main functions for earlier version, targeting Scribo.Lilypond output 

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
        voices         = map (\v -> ark2voice arca config penult sylCount lineCount v perm) 
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
                symphonia   = map (stepwiseVoiceInRange $ ranges arca) merged 
                merged      = mergeChoruses choruses 
                choruses    = map (\(p,s) -> getChorus arca config p s) permPhrases
                permPhrases = zip (phrases sentence) perms
    
-- | Get all the music for the sections from input
getMasterMusic :: Arca -> [LyricSection] -> [SectionPerm] -> [Symphonia]
getMasterMusic arca sections perms = 
    map (\ (s,p) -> getSymphonia arca s p) $ zip sections perms

-- End Lilypond section ------------------------------------------------


