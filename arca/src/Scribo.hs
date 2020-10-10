{- |
 Scribo: Writing output of Kircher's Arca to music-notation language
-}

module Scribo where

import Data.List
import Arca 
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
voice2ly :: Voice -> Phrase -> String
voice2ly v p = enbrace contents "\\new Staff <<\n \\new Voice " ">>\n" 
    where 
        contents = voicename ++ lyMusic ++ lyLyrics
        voicename = enbrace (show id) "= \"" "\" "
        lyMusic  = lyMusicGroup $ getClef id ++ notes
        notes    = unwords (map pitch2ly (music v))
        lyLyrics = lyrics2ly p id
        id       = voiceID v

-- TODO add time signature

lyrics2ly :: Phrase -> VoiceName -> String
lyrics2ly phrase voice = enbrace contents "\\new Lyrics " "\n"
    where 
        contents    = voicename ++ lyrics
        voicename   = enbrace (show voice) "\n\\lyricsto \"" "\" "
        lyrics      = enbrace syllables "{ \\lyricmode {\n" "\n}\n}"
        syllables   = unwords $ map (\ v -> intercalate " -- " v) text
        text        = map verbumSyl $ phraseText phrase


getClef :: VoiceName -> String
getClef v = enbrace clefName "\\clef \"" "\"\n"
    where clefName = case v of
            Soprano -> "treble"
            Alto    -> "treble"
            Tenor   -> "treble_8"
            Bass    -> "bass"

lyVersionString = "2.20"

lyVersion :: String -> String
lyVersion s = enbrace s "\\version \"" "\"\n"

-- | Write a chorus to a Lilypond simultaneous group
chorus2ly :: Chorus -> Phrase -> String
chorus2ly ch ph = lySimultaneousGroup $ unwords $ map (\ c -> voice2ly c ph) ch

-- | Run the whole machine in one go.
compose :: Arca -> Style -> Meter -> Perm -> Phrase -> String
compose arca style meter perm phrase = lyCmd
    where 
        lyCmd    = lyVersion lyVersionString ++ lyScore
        lyScore  = enbrace lyStaves "\\score {\n<<\n" ">>\n}\n"
        lyStaves = enbrace lyChorus "\\new ChoirStaff\n" "\n"
        lyChorus = chorus2ly chorus phrase
        chorus   = getChorus arca style meter perm phrase -- from Cogito

-- TODO add ly header

-- TODO chorus vs [[Voice]]
-- arca2ly :: [Chorus] -> String
-- arca2ly music = lyCmd
--     where 
--         lyCmd    = lyVersion lyVersionString ++ lyScore
--         lyScore  = enbrace lyStaves "\\score {\n<<\n" ">>\n}\n"
--         lyStaves = enbrace lyChorus "\\new ChoirStaff\n" "\n"
--         lyChorus = chorus2ly chorus
--         chorus   = pivot music



