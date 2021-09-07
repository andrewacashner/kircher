{-|
Module      : Cogito
Description : Process ark input to create music
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module processes data from the ark to convert it into music (/cogito/,
Latin, "I think").

= Overview

This module receives input from the @Lectio@ module in the form of a single
list of one or more 'LyricSection's, which contain the parsed text to be set
to music and the parameters for setting it.

The module uses Kircher's rules to pull the appropriate data from the Arca
musarithmica, that is, from the 'Arca' built by the @Aedifico@ module.  It
uses the @Fortuna@ module to get lists of random permutation indices.

The main function is 'makeMusicScore', which applies all the necessary rules to
select music data from the ark for each phrase of text, using the random
permutations when a free choice would otherwise be needed. It takes the
numerals and rhythmic symbols from Kircher's /pinakes/ (rods); converts the
numerals to pitches according to the mode, and combines the pitches and
rhythms (accounting for rests as well).

The module creates the 'MusicScore' data structure which contains all the data
for the music in hierarchical sections that the @Scribo@ module will convert
to MEI XML. 

== Text underlay

Pitches and syllables are stored together in the 'Note' type. In Syntagma I
(simple syllabic counterpoint), we store one syllable for each note, skipping
rests. 

In Syntagma II, though, for florid counterpoint, Kircher does not specify how
to underlay the text, and the settings have variable numbers of notes in the
different voices and between different permutations in the same /pinax/. The
underlay must be left to the human performer, then, and so we just lump all
the lyrics for each phrase and put them under the first syllable as a textual
incipit.

== MEI vs. Lilypond output

We previously set up this module to feed data into the @Scribo.Lilypond@
module, using the main function @getSymphonia@. It treated pitches and lyrics
completely separately, which worked in Syntagma I but not in Syntagma II.
These functions are archived in the @test/@ directory. 
-}


module Cogito where

import Data.List.Index as I
    (indexed)

import Aedifico 
    ( Accid        (..)
    , AccidType    (..)
    , Arca         (..)
    , ArkConfig    (..)
    , Dur          (..)
    , Mode         (..)
    , ModeList     (..)
    , ModeSystem   (..)
    , PenultLength (..)
    , Pitch        (..)
    , Pnum         (..)
    , Style        (..)
    , VoiceName    (..)
    , VoiceRanges  (..)
    , TextMeter    (..)
    , getVectorItem
    , getVoice
    , getRperm
    , proseMeter
    , modeOrModeB
    )

import Cogito.Musarithmetic
    ( MusicScore
    , MusicSentence
    , MusicChorus       (..)
    , MusicSection      (..) 
    , MusicPhrase       (..)
    , Note              (..)
    , Syllable          (..)
    , SyllablePosition  (..)
    , Voice             (..)
    , isRest
    , isPitchRest
    , modeMollis
    , modalFinal
    , newRest
    , p7inc
    , pnumAccidInMode
    , stepwiseVoiceInRange
    )
 
import Cogito.Ficta
    (adjustFictaChorus)

import Fortuna 
    ( Perm (voiceIndex, rhythmIndex)
    , SectionPerm
    , SentencePerm
    )

import Lectio

-- * Match pitches and rhythms 

-- ** Get music data for a single voice

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

-- | Make a pitch from duration and pitch number. Start with zero octave;
-- we'll set it later using 'stepwiseVoiceInRange'.
-- Adjust the pitch for mode ('pnumAccidInMode').
--
-- __TODO__: This could also be generalized; we are not checking inputs
-- because we control data input.
pair2Pitch :: ModeList
           -> ModeSystem
           -> Mode
           -> (Dur, Int) -- ^ duration and pitch number 0-7
           -> Pitch
pair2Pitch modeList systems mode pair | isRest thisDur = newRest thisDur 
                                      | otherwise      = newPitch
    where 
        newPitch = Pitch {
            pnum      = thisPnumInMode,
            accid     = thisAccid,
            accidType = thisAccidType,
            oct       = 4, -- dummy value, will be adjusted
            dur       = thisDur
        } 
        thisPnum  = (snd pair) - 1 -- adjust to 0 index
        thisDur   = fst pair

        thisPnumInMode = fst modePitch
        thisAccid      = snd modePitch
        modePitch      = pnumAccidInMode thisPnum modeList mode

        thisAccidType = case thisAccid of
            Na -> Implicit
            Sh -> Suggested
            Fl -> if isBflatInSignature thisPnumInMode thisAccid mode systems 
                    then Implicit 
                    else Suggested
            _  -> None
            
        pitchOffsetFromFinal = final `p7inc` thisPnum
        final                = modalFinal modeList mode 

-- | Is this note a B flat, and if so, is the flat already in the key
-- signature?
isBflatInSignature :: Pnum -> Accid -> Mode -> ModeSystem -> Bool
isBflatInSignature pnum accid mode systems = 
    pnum == PCb 
    && accid == Fl
    && modeMollis mode systems

-- | Get the right starting octave range for each voice type voice2octave :: VoiceName -> Int
voice2octave v = case v of
    Soprano -> 4
    Alto    -> 3
    Tenor   -> 3
    Bass    -> 2


-- * From input parameters to music

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
        newMusic    = map (pair2Pitch modeList modeSystems mode) pairs
        vocalRanges = ranges arca
        modeList    = modes arca
        modeSystems = systems arca
        mode        = modeOrModeB config lineCount
        style       = arkStyle config
        meter       = arkTextMeter config

        pairs       = zipFill rperm vpermVoice isRest $ fromEnum Rest

        -- In syntagma 1 there is only one rperm for all four vperm voices;
        -- in syntagma 2 we match the four rperms to the four vperm voices.
        rperm       = case style of
                         Simple -> getVectorItem "ark2voice:rpermChoir" rpermChoir 0
                         Florid -> getVectorItem "ark2voice:rpermChoir" rpermChoir $ fromEnum voice
        
        vpermVoice  = getVoice arca newConfig sylCount lineCount voice vpermNum
        rpermChoir  = getRperm arca newConfig sylCount lineCount rpermNum

        newConfig   = ArkConfig {
            arkStyle        = style,
            arkMode         = arkMode config,
            arkModeB        = arkModeB config,
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


-- * Methods to create and populate data structures for music composed by the
-- ark

-- | Take a 'Verbum' read from the input file and turn it into a list of
-- 'Syllable's for storage in 'Note's. Record the syllable's position within
-- the word.
makeSyllables :: Verbum -> [Syllable]
makeSyllables word = map (\(i, syl) -> Syllable {
        sylText = syl,
        sylPosition  = position i
    }) $ I.indexed $ verbumSyl word
    where 
        position :: Int -> SyllablePosition
        position i | sylCount word == 1         = Only
                   | i == 0                     = First
                   | i == (sylCount word - 1)   = Last
                   | otherwise                  = Middle

-- | Just a blank syllable for filler when needed
blankSyllable :: Syllable
blankSyllable = Syllable "" Tacet

-- | Compose the music for a whole 'LyricPhrase' with one permutation from the
-- ark, and package it into a 'MusicPhrase'. Note that this is for a single
-- voice only, not the four SATB voices. 
--
-- Line up pitches and syllables, skipping rests. In Syntagma I, line up text
-- and notes syllabically (one syllable per note); in syntagma II (florid),
-- lump the text into a single syllable and put it as an incipit text at the
-- beginning of the phrase. (See module description for why Kircher's
-- specification makes this is necessary.)
makeMusicPhrase :: Arca 
                    -> ArkConfig 
                    -> VoiceName
                    -> LyricPhrase 
                    -> Perm 
                    -> MusicPhrase
makeMusicPhrase arca config voiceID phrase perm = MusicPhrase {
        phraseVoiceID = voiceID,
        notes = theseNotes
    } where

        -- Match up pitches and syllables, skipping rests
        theseNotes = map (\(pitch, syllable) -> Note pitch syllable)
            $ zipFill (music voice) syllables isPitchRest blankSyllable

        voice       = stepwiseVoiceInRange (ranges arca) voiceRaw :: Voice
        voiceRaw    = ark2voice arca config penult sylCount lineCount voiceID perm

        range       = ranges arca
        penult      = phrasePenultLength phrase
        sylCount    = phraseSylCount phrase
        lineCount   = phrasePosition phrase

        words = phraseText phrase
        
        -- In Syntagma II, put the whole phrase of lyrics as a single
        -- syllable under the first note
        syllables = case arkStyle config of
            Simple -> concat $ map makeSyllables words
            Florid -> [Syllable {
                        sylText = unwords $ map verbumText $ phraseText phrase,
                        sylPosition = Only
                       }]

-- | Compose music for a 'LyricSentence' for a single voice.
makeMusicSentence :: Arca 
                    -> ArkConfig 
                    -> VoiceName
                    -> LyricSentence 
                    -> SentencePerm 
                    -> MusicSentence
makeMusicSentence arca config voiceID sentence sentencePerms = 
    zipWith (makeMusicPhrase arca config voiceID) 
    (phrases sentence) sentencePerms

-- | Put together all the music information for one 'LyricSection', for a
-- single voice.
--
-- * For a single voice:
--
--      * extract ArkConfig for whole section
--
--      * for each sentence in section:
--
--          * extract list of perms, one per phrase
--          * extract list of lyric phrases
--          * apply same ArkConfig
--
--      * for each phrase in sentence:
--
--          * look up vperm according to config and perm
--
--              * (for some pinakes, choose column by stanza = section num)
--          
--          * look up rperm according to config and perm
--              
--              * (for syntagma II, use same perm)
--      
--          * convert vperm nums to pitch names
--          * (adjust pitches)
--          * make Pitches: match pitches and rhythms, accounting for rests
--          
--          * match Notes: match each Pitch with Phrase/Verbum/Syllable
--                          according to syntagma
--          
--          * return a MusicPhrase
--
--      * inside a MusicSentence
--
--  * inside a MusicSection
makeMusicSection :: Arca 
                    -> LyricSection 
                    -> SectionPerm 
                    -> VoiceName
                    -> MusicSection
makeMusicSection arca section sectionPerms voiceID = MusicSection {
        secVoiceID      = voiceID,
        secConfig       = sectionConfig $ section,
        secSentences    = sentenceList
    } 
    where 
        sentenceList = zipWith (makeMusicSentence arca config voiceID)
                        (sentences section) sectionPerms
        config = sectionConfig section

-- | Compose music for all four SATB voices for one 'LyricSection'.
-- TODO experimental: also adjust for musica ficta
makeMusicChorus :: Arca
                    -> LyricSection
                    -> SectionPerm
                    -> MusicChorus
makeMusicChorus arca section perm = adjustFicta -- rawChorus 
    where
        adjustFicta = adjustFictaChorus (systems arca) (modes arca) rawChorus
        rawChorus   = MusicChorus {
            soprano = makesec Soprano,
            alto    = makesec Alto,
            tenor   = makesec Tenor,
            bass    = makesec Bass
        }
        makesec = makeMusicSection arca section perm 

-- | Compose the music for the whole document as a 'MusicScore', pulling the
-- data from the 'Arca'.
makeMusicScore :: Arca
                    -> [LyricSection]
                    -> [SectionPerm]
                    -> MusicScore
makeMusicScore arca lyricSections sectionPerms = 
    zipWith (makeMusicChorus arca) lyricSections sectionPerms 



