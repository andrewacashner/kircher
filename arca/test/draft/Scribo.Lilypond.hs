{-|
Module      : Scribo.Lilypond
Description : Write output of the ark to Lilypond music-notation language
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module is our implementation of Kircher's /palimpsest phonotacticum/, his
system for writing out the music created using the ark.  Certain elements that
Kircher used notation to calculate (like determining vocal ranges by clef
combinations and the size of the staff) we actually do in the @Cogito@ module.
This module is purely focused on output of complete music information to a
music-notation language.

This module outputs to Lilypond, which could then be processed by that program
to PDF, MIDI, or other formats. 

This module also contains the central 'compose' function that takes in a text
processed by the @Lectio@ module, and does the rest of the work of getting
data from the ark, adjusting and translating that data into music (using
@Cogito@), and then writing it.

-}


module Scribo.Lilypond where

import Data.List 
    ( intercalate
    , transpose
    )

import Aedifico
    ( Pnum       (..)
    , Accid      (..)
    , VoiceName  (..)
    , Dur        (..)
    , MusicMeter (..)
    , Style      (..)
    , ArkConfig  (..)
    , ModeList
    , ModeSystem
    , Arca       (..)
    , Pitch     (..)
    )

import Cogito 
    ( Voice (voiceID, music)
    , Chorus
    , Symphonia (..)
    , isRest
    , isPitchRest
    , modeMollis
    , getMasterMusic
    )

import Fortuna 
    ( Perm
    , SectionPerm
    )

import Lectio 
    ( LyricSentence   (..)
    , LyricSection    (..)
    , ArkMetadata     (..)
    , LyricPhrase     (..)
    , Verbum          (..)
    )

-- * Write individual data types to Lilypond strings

-- | Write pitch as Lilypond music note.
-- Look up needed string values for letter name, accidental, octave tick marks,
-- and duration in lists based on data in given 'Pitch'.
-- If it is a 'Aedifico.Pnum.Rest', just print the rest rhythm string.
--
-- Most of these just require using an enum value as index to a list of
-- strings or characters. The octave requires us to calculate the number of
-- commas or apostrophes to add (relative to Helmholtz octave 3 = @c@).
pitch2ly :: Pitch -> String
pitch2ly p =
    if isRest $ dur p
        then duration
        else if accid p /= Na
            then "\\ficta " ++ pitchLetter ++ accidental ++ octaveTicks ++ duration
            else pitchLetter ++ octaveTicks ++ duration
    where
        duration    = case (dur p) of
            Br  -> "\\breve"
            Sb  -> "1"
            Mn  -> "2"
            Sm  -> "4"
            Fs  -> "8"
            BrD -> "\\breve."
            SbD -> "1."
            MnD -> "2."
            SmD -> "4."
            FsD -> "8."
            LgR -> "r\\longa"
            BrR -> "r\\breve"
            SbR -> "r1"
            MnR -> "r2"
            SmR -> "r4"
            FsR -> "r8"

        pitchLetter = case (pnum p) of
            PCc  -> "c"
            PCd  -> "d"
            PCe  -> "e"
            PCf  -> "f"
            PCg  -> "g"
            PCa  -> "a"
            PCb  -> "b"
            PCc8 -> "c"

        accidental  = case (accid p) of
            Fl  -> "es"
            Na  -> ""
            Sh  -> "is"
            AccidNil -> ""

        octaveTicks = oct2str $ oct p

        oct2str :: Int -> String
        oct2str oct  
            | oct < 3   = replicate degree low 
            | oct > 3   = replicate degree high 
            | otherwise = ""
            where 
                low = ','
                high = '\''
                degree = abs (oct - 3)


-- * Write larger structures of music data to Lilypond, with necessary framing
-- inside @\score@, @\Staff@, and so on.

-- | Put things inside enclosing parens, brackets, etc.
enbrace :: String -- ^ string to put between enclosures
        -> String -- ^ opening enclosure text (e.g., @{@)
        -> String -- ^ closing text (e.g., @}@)
        -> String
enbrace str open close = open ++ str ++ close

-- | Group string in curly braces @{string}@ with added newlines
lyMusicGroup :: String -> String
lyMusicGroup str = enbrace str "{\n" "\n}\n"

-- | Group string in double angle brackets @\<\<string\>\>@ with added newlines
lySimultaneousGroup :: String -> String
lySimultaneousGroup str = enbrace str "<<\n" "\n>>\n"

-- | Write a 'Voice' to a Lilypond music group:
--
--      @\\new Staff\<\< \\new Voice { ... } \>\>@
--
-- We have to include Lilypond @midiInstrument@ here.
voice2ly :: Voice -> ModeSystem -> LyricSection -> String
voice2ly voice modeSystem section = lyMusicMeter ++ lyKey ++ notes
    where
        notes     = unwords (map pitch2ly $ music voice)

        id        = voiceID voice
        meter     = arkMusicMeter config
        mode      = arkMode config
        config    = sectionConfig section
       
        lyMusicMeter = enbrace meterName "\\time " "\n" 
        meterName    = case meter of
            Duple       -> "2/2"
            TripleMajor -> "3/1"
            TripleMinor -> "3/2"

        lyKey 
            | modeMollis mode modeSystem = "\\key f\\major\n"
            | otherwise       = ""

-- | Write a 'LyricSection' to a Lilypond @\new Lyrics { }@ statement for a
-- particular voice (@VoiceName@). Separate -- syllables with @ -- @.
lyrics2ly :: LyricSection -> Voice -> String
lyrics2ly section voice = 
    case (arkStyle $ sectionConfig section) of
        Simple -> syllableString
        Florid -> wordString
    where 
        -- In Syntagma I we line up one syllable per note and insert "--" to
        -- get hyphens between syllables in words
        syllableString = unwords $ map (\v -> intercalate " -- " v) lsSyllables
        lsSyllables    = map verbumSyl lsVerba
        lsVerba        = concat $ map phraseText lsPhrases
        lsPhrases      = concat $ map phrases $ sentences section 

        -- In Syntagma II the florid voices screw up syllabification and
        -- Kircher does not specify an algorithm for text underlay.
        -- __ TODO __ 
        -- Fixing this will require you to change Symphonia to include a list
        -- of Choruses instead of a single Chorus so that it matches up with
        -- the structure of the lyric text in LyricSentences; 
        -- you will need to process music and lyrics for each phrase and THEN
        -- adjust the music (stepwise, etc.) and combine
        wordString | voiceID voice == Bass  = lyricIncipits voice section
                   | otherwise              = blankLyrics voice section

        -- | Put a filler lyric ("_") under each note that is not a rest.
        blankLyrics :: Voice -> LyricSection -> String
        blankLyrics voice section = unwords $ replicate sylCount "_"
            where
                sylCount        = length pitchesNotRests
                pitchesNotRests = filter (not . isPitchRest) $ music voice

        -- | Make each phrase of lyrics into a single-word string, put them
        -- all in a row at the beginning of the section, and fill the rest
        -- with blanks.
        -- __ TODO __ this is a placeholder: we need to align each string with
        -- the start of a music perm.
        lyricIncipits :: Voice -> LyricSection -> String
        lyricIncipits voice section = unwords [incipits, blanks]
            where
                blanks          = unwords $ replicate (sylCount - sum lengths) "_"
                sylCount        = length pitchesNotRests
                pitchesNotRests = filter (not . isPitchRest) $ music voice
                incipits        = unwords $ map (unwords . map lumpWords . phrases) 
                                    $ sentences section
                lengths         = map sentenceLength $ sentences section

        -- | Combine lyrics for a phrase into a single word with "_" elisions
        -- between words, for use as a lyrical incipit
        lumpWords :: LyricPhrase -> String
        lumpWords p = intercalate "_" $ map verbumText $ phraseText p

-- | The opening string per voice
voice2lyOpening :: Voice -> String
voice2lyOpening voice = unwords 
    ["\\new Staff \\with { midiInstrument = \"choir aahs\" }\n ",
     "<<\n\\new Voice = \"" ++ show id ++ "\" {\n",
     lyClef]
    where 
        id        = voiceID voice
        lyClef    = enbrace clefName "\\clef \"" "\"\n" 
        clefName  = case id of
            Soprano -> "treble"
            Alto    -> "treble"
            Tenor   -> "treble_8"
            Bass    -> "bass"

-- | The end of the music part of the voice and beginning of the lyrics
voice2lyClosing :: Voice -> String
voice2lyClosing voice = "\\FinalBar\n}\n" ++ "\\new Lyrics \\lyricsto \"" 
        ++ (show $ voiceID voice) ++ "\" {\n\\lyricmode {\n"
     
-- | The end of the voice and its lyrics (discards input)
voiceClosing :: Voice -> String
voiceClosing voice = "}\n}\n>>\n"


-- | Convert all the music derived from input into notation output
masterMusic2ly :: Arca -> [Symphonia] -> String
masterMusic2ly arca symphoniae = lySimultaneousGroup $ notes
    where
        notes   = unwords $ concat 
                    $ transpose [starts, middles, ends, lyrics, close]

        firstChorus = chorus $ head symphoniae
        
        -- one of these per voice (list of four)
        starts  = map voice2lyOpening firstChorus
        ends    = map voice2lyClosing firstChorus
        close   = map voiceClosing firstChorus

        -- get the music for each section, then combine the music for each
        -- voice to end up with a list of four voices
        middles = sectionString 
            $ map (\ s -> voice2lySection s $ systems arca) symphoniae
        
        -- likewise but for lyrics
        lyrics = sectionString $ map lyrics2lySection symphoniae
        
        sectionString :: [[String]] -> [String]
        sectionString = (map unwords) . transpose 

        voice2lySection:: Symphonia -> ModeSystem -> [String]
        voice2lySection s modes = 
            map (\v -> voice2ly v modes $ lyricSection s) $ chorus s

        lyrics2lySection :: Symphonia -> [String]
        lyrics2lySection s = 
            map (\v -> lyrics2ly (lyricSection s) v) $ chorus s



-- | Make Lilypond preamble of include commands
makePreamble :: [String] -> String
makePreamble includes = unwords $ map (\ s -> "\\include \"" ++ s ++ "\"\n") includes

-- | Make Lilypond header with the Arca as the author
makeHeader :: ArkMetadata -> String
makeHeader metadata = "\\header {\ntitle = \"" ++ arkTitle metadata ++ 
    "\"\npoet = \"" ++ arkWordsAuthor metadata ++ 
    "\"\ncomposer = \\markup{\n"++
    "\\column{" ++
    "\\line {\"Arca musarithmica Athanasii Kircheri\"}\n" ++
    "\\line {\"Societatis Iesu MMXX\"}\n" ++
    "}\n}\ntagline = ##f\n}"

-- * All together now

-- | Set a prepared 'LyricSentence' to music in one go. Return the text of a
-- complete Lilypond file as a string. Write version number and preamble, and
-- put music into @score@ and @StaffGroup@ (needed because we are doing
-- /Mensurstriche/).
--
compose :: Arca     
        -> ArkMetadata
        -> [LyricSection]
        -> [SectionPerm]
        -> String   
compose arca metadata sections perms = lyCmd
    where 
        lyCmd      = lyVersion ++ lyPreamble ++ lyHeader ++ lyScore

        lyVersion  = enbrace lyVersionString "\\version \"" "\"\n"
        lyVersionString = "2.23"

        lyPreamble = makePreamble ["early-music.ly", "mensurstriche.ly"]

        lyHeader   = makeHeader metadata 

        lyScore    = enbrace lyStaves "\\score {\n<<\n" $ ">>\n" ++ lyMidi ++ "}\n"
        lyMidi     = "\\layout{}\n\\midi{\\tempo 2 = 120}\n"

        lyStaves   = enbrace lyChorus "\\new StaffGroup\n" "\n"

        lyChorus   = masterMusic2ly arca symphoniae 
        symphoniae = getMasterMusic arca sections perms
        




