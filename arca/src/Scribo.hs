{- |
 Scribo: Writing output of Kircher's Arca to music-notation language
-}

module Scribo where

import Data.List
import Aedifico
import Cogito
import Fortuna
import Lectio

-- * Write to Lilypond
-- | Write pitch as Lilypond music note.
-- Look up needed string values for letter name, accidental, octave tick marks,
-- and duration in lists based on data in given @Pitch@.
-- If it is a rest, just print the rest rhythm string.
pitch2ly :: Pitch -> String
pitch2ly (Pitch pnum oct dur accid) =
    if isRest dur
        then duration
        else pitchLetter ++ accidental ++ octaveTicks ++ duration
    where
        pitchLetter = pitch2str pnum
        accidental  = accid2str accid
        octaveTicks = oct2str oct
        duration    = dur2str dur

-- ** Convert individual pitch data members to strings
dur2str :: Dur -> String
dur2str dur = durValues !! fromEnum dur
    where durValues = 
            ["\\breve", "1", "2", "4", "8",
            "\\breve.", "1.", "2.", "4.", "8.",
            "r\\breve", "r1", "r2", "r4", "r8"]

pitch2str :: Pnum -> String
pitch2str pnum = pitchLetter !! fromEnum pnum
    where pitchLetter = words "c d e f g a b"

accid2str :: Accid -> String
accid2str accid = suffix !! fromEnum accid
    where suffix = ["es", "", "is", ""] 

-- | Create tick marks to show octave (Helmholtz oct 4 = c')
oct2str :: Int -> String
oct2str oct  
    | oct < 3   = replicate degree low 
    | oct > 3   = replicate degree high 
    | otherwise = ""
    where 
        low = ','
        high = '\''
        degree = abs (oct - 3)


-- *** Put things inside enclosing parens, brackets, etc.
enbrace :: String -> String -> String -> String
enbrace str open close = open ++ str ++ close

-- | Group in curly braces { }
lyMusicGroup :: String -> String
lyMusicGroup str = enbrace str "{\n" "\n}\n"

-- | Group in double angle brackets << >>
lySimultaneousGroup :: String -> String
lySimultaneousGroup str = enbrace str "<<\n" "\n>>\n"

-- ** Write whole chunk to a single Lilypond string
-- | Write a voice to Lilypond music group
voice2ly :: Voice -> Meter -> Sentence -> String
voice2ly voice meter sentence = enbrace contents "\\new Staff <<\n \\new Voice " ">>\n" 
    where 
        contents  = voicename ++ lyMusic ++ lyLyrics
        voicename = enbrace (Prelude.show id) "= \"" "\" "
        lyMusic   = lyMusicGroup $ getClef id ++ getMeter meter ++ notes ++ finalBar
        notes     = unwords (map pitch2ly (music voice))
        lyLyrics  = lyrics2ly sentence id
        id        = voiceID voice

-- TODO add time signature

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


getClef :: VoiceName -> String
getClef v = enbrace clefName "\\clef \"" "\"\n"
    where clefName = case v of
            Soprano -> "treble"
            Alto    -> "treble"
            Tenor   -> "treble_8"
            Bass    -> "bass"

getMeter :: Meter -> String
getMeter m = enbrace meterName "\\time " "\n"
    where meterName = case m of
            Duple       -> "4/2"
            TripleMajor -> "3/1"
            TripleMinor -> "3/2"

finalBar = "\\FinalBar\n"

lyVersionString = "2.20"

lyVersion :: String -> String
lyVersion s = enbrace s "\\version \"" "\"\n"

lyPreamble = "\\include \"mensurstriche.ly\"\n"

-- | Write a chorus to a Lilypond simultaneous group
chorus2ly :: Chorus -> Meter -> Sentence -> String
chorus2ly ch meter ph = lySimultaneousGroup $ unwords $ map (\ c -> voice2ly c meter ph) ch

-- | Run the whole machine in one go. Set one phrase of text to music given
-- settings for one perm.
compose :: Arca -> Style -> Meter -> [Perm] -> Sentence -> String
compose arca style meter perms sentence = lyCmd
    where 
        lyCmd    = lyVersion lyVersionString ++ lyPreamble ++ lyScore
        lyScore  = enbrace lyStaves "\\score {\n<<\n" ">>\n}\n"
        lyStaves = enbrace lyChorus "\\new StaffGroup\n" "\n"
            -- mensurstriche requires StaffGroup not ChoirStaff
        lyChorus = chorus2ly symphonia meter sentence
        symphonia = Cogito.getSymphonia arca style meter perms sentence 

-- TODO add ly header (title, author, date)

