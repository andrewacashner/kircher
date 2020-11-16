{-|
Module      : Scribo 
Description : Write output of the ark to music-notation language
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

__TODO__: Output to MEI-XML instead?
-}


module Scribo where

import Data.List 
    (intercalate)

import Aedifico
    (Pnum       (..),
     Accid      (..),
     VoiceName  (..), 
     Dur        (..),
     Meter      (..),
     Style,
     ArkConfig  (..),
     ModeList,
     ModeSystem,
     Arca       (..),
     Pitch  (..))

import Cogito 
    (Voice (voiceID, music),
     Chorus,
     isRest,
     modeMollis,
     getSymphonia)

import Fortuna 
    (Perm)

import Lectio 
    (Sentence (..), 
     Phrase   (phraseText), 
     Verbum   (verbumSyl))

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
voice2ly :: Voice -> ModeSystem -> Sentence -> String
voice2ly voice modeSystem sentence = enbrace contents "\\new Staff <<\n \\new Voice " ">>\n" 
    where 
        contents  = voicename ++ lyMusic ++ lyLyrics
        voicename = enbrace (show id) "= \"" "\" "
        lyMusic   = lyMusicGroup $ lyClef ++ lyMeter ++ lyKey ++ notes ++ finalBar
        notes     = unwords (map pitch2ly $ music voice)
        lyLyrics  = lyrics2ly sentence id
        id        = voiceID voice
        meter     = arkMeter $ arkConfig sentence
        mode      = arkMode  $ arkConfig sentence
       
        finalBar  = "\\FinalBar\n"
        
        lyClef    = enbrace clefName "\\clef \"" "\"\n" 
        clefName  = case id of
            Soprano -> "treble"
            Alto    -> "treble_8"
            Tenor   -> "treble_8"
            Bass    -> "bass"

        lyMeter   = enbrace meterName "\\time " "\n" 
        meterName = case meter of
            Duple       -> "4/2"
            TripleMajor -> "3/1"
            TripleMinor -> "3/2"

        lyKey 
            | modeMollis mode modeSystem = "\\key f\\major\n"
            | otherwise       = ""

-- | Write a 'Sentence' to a Lilypond @\new Lyrics { }@ statement for a
-- particular voice (@VoiceName@). Separate -- syllables with @ -- @.
lyrics2ly :: Sentence -> VoiceName -> String
lyrics2ly sentence voice = enbrace contents "\\new Lyrics " "\n"
    where 
        contents       = voicename ++ lyrics
        voicename      = enbrace (Prelude.show voice) "\n\\lyricsto \"" "\" "
        lyrics         = enbrace syllableString "{ \\lyricmode {\n" "\n}\n}"
        syllableString = unwords $ map (\ v -> intercalate " -- " v) lsSyllables
        lsSyllables    = map (\ v -> verbumSyl v) lsVerba
        lsVerba        = concat $ map (\ p -> phraseText p) lsPhrases
        lsPhrases      = phrases sentence 

-- | Write a 'Chorus' of music matching text in 'Sentence', in a given 'Meter'
-- to a Lilypond simultaneous group.
chorus2ly :: Arca -> Chorus -> Sentence -> String
chorus2ly arca chorus sentence = lySimultaneousGroup $ unwords notes
    where 
        notes    = map (\ voice -> voice2ly voice modeSystem sentence) chorus
        modeSystem = systems arca 

-- | Make Lilypond preamble of include commands
makePreamble :: [String] -> String
makePreamble includes = unwords $ map (\ s -> "\\include \"" ++ s ++ "\"\n") includes

-- * All together now

-- | Set a prepared 'Sentence' to music in one go. Return the text of a
-- complete Lilypond file as a string. Write version number and preamble, and
-- put music into @score@ and @StaffGroup@ (needed because we are doing
-- /Mensurstriche/).
--
-- __TODO__: add ly header (title, author, date)
compose :: Arca     -- ^ structure created in @Arca_musarithmica@ using @Aedifico@
        -> Sentence -- ^ created from input text using @Lectio@, includes 'ArkConfig'
        -> [Perm]   -- ^ list of @Perm@s same length as 'Sentence' phrases, from @Fortuna@
        -> String   -- ^ Complete Lilypond file
compose arca sentence perms = lyCmd
    where 
        lyCmd     = lyVersion ++ lyPreamble ++ lyScore
        lyVersion = enbrace lyVersionString "\\version \"" "\"\n"
        lyScore   = enbrace lyStaves "\\score {\n<<\n" ">>\n}\n"
        lyStaves  = enbrace lyChorus "\\new StaffGroup\n" "\n"
        lyChorus  = chorus2ly arca symphonia sentence
        symphonia = getSymphonia arca sentence perms 
        
        lyVersionString = "2.20"
        lyPreamble      = makePreamble ["early-music.ly", "mensurstriche.ly"]




