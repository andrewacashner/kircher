{-|
Module      : Scribo.MEI
Description : Write output of the ark in MEI-XML music encoding
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module is our implementation of Kircher's /palimpsest phonotacticum/, his
system for writing out the music created using the ark.  Certain elements that
Kircher used notation to calculate (like determining vocal ranges by clef
combinations and the size of the staff) we actually do in the @Cogito@ module.
This module is purely focused on output of complete music information to a
music-notation language.

This module outputs in the XML format of the Music Encoding Initiative (MEI).

This means that the whole arca program is transforming an input XML document with the text and parameters into and output XML document with the text matched to music.
-}

module Scribo.MEI where

import Data.List 
    (transpose)

import Aedifico
import Lectio
import Cogito


-- * Write XML 

-- ** Utilities

-- | Put a string between two other strings
enbrace :: String   -- ^ start
        -> String   -- ^ end
        -> String   -- ^ contents to go in between
        -> String
enbrace start end contents = start ++ contents ++ end

-- | Create an XML opening tag, e.g., @<p>@
xmltagOpen :: String -- ^ text of tag
            -> String
xmltagOpen name = enbrace "<" ">" name

-- | Create an XML closing tag, e.g., @</p>@
xmltagClose :: String -- ^ text of tag
            -> String
xmltagClose name = enbrace "</" ">" name

-- | Put something between opening and closing XML tags, specifying text of
-- opening tag, contents, and closing tag:
-- > xmlWrap "p" "text" "p" => <p>text</p>
xmlWrap :: String   -- ^ opening tag text
        -> [String] -- ^ list of contents
        -> String   -- ^ closing tag text
        -> String
xmlWrap open contents close = xmltagOpen open 
                                ++ unwords contents 
                                ++ xmltagClose close

-- | Put something between XML tags, with the same text for opening and
-- closing: 
-- > xmlWrapBasic "p" "text" => <p>text</p>
xmlWrapBasic :: String  -- ^ open and closing tag text
            -> [String] -- ^ list of contents
            -> String
xmlWrapBasic name contents = xmlWrap name contents name

-- ** Create elements and attributes, and elements with attributes

-- | Create an XML element (which may contain other elements).
-- If you need attributes, use 'elementAttr'.
element :: String   -- ^ name
        -> [String] -- ^ values (list of contents, including other elements)
        -> String
element name values = xmlWrapBasic name values 

-- | Create an XML attribute
attr :: String  -- ^ name
    -> String   -- ^ single value
    -> String
attr name value = name ++ enbrace "=\"" "\"" value

-- | Create an XML element that has attributes, e.g., @<p class="foo">bar</p>@
elementAttr :: String   -- ^ name
            -> [String] -- ^ list of attributes created with 'attr'
            -> [String] -- ^ values (list of contents, including other elements)
            -> String
elementAttr name attrs values = xmlWrap (unwords [name, unwords attrs]) values name


-- * Serialize our data structures to MEI

-- ** 'Note' becomes @note@ (pitch with syllable, or rest)

-- | Create an MEI @note@ element from our @Note@ datatype, converting the
-- attributes as needed. If the @Note@ actually contains a rest ('Pitch' with
-- only a 'Dur' in the rest range), then produce a @rest@ element.
note2mei :: Note -> String
note2mei note | isPitchRest pitch = meiRest
              | otherwise         = meiNote 
    where 
        pitch    = notePitch note
        syllable = noteSyllable note

        meiRest  = elementAttr "rest" [meiDur pitch] []
        
        meiNote  = elementAttr "note"
                    [fn pitch | fn <- [meiPname, meiOct, meiDur, meiAccid]]
                    [element "verse" [meiSyllable syllable]]
        
-- *** Conversions for 'Note' members

-- | Convert 'Pnum' to MEI @pname@
meiPname :: Pitch -> String
meiPname p = attr "pname" [c] 
    where c = "cdefgabc_" !! (fromEnum $ pnum p)

-- | Just print the octave number
meiOct :: Pitch -> String
meiOct p = attr "oct" $ show $ oct p

-- | Convert 'Dur' to MEI @dur@ (base value of duration) and @dots (if any;
-- omit if not)
meiDur :: Pitch -> String
meiDur p = unwords [durAttr, dotsAttr]
    where 
        durAttr = attr "dur" $ durString $ dur p

        durString :: Dur -> String
        durString d | d == DurNil             = "_"
                    | d `elem` [Br, BrD, BrR] = "breve"
                    | d `elem` [Sb, SbD, SbR] = "1"
                    | d `elem` [Mn, MnD, MnR] = "2"
                    | d `elem` [Sm, SmD, SmR] = "4"
                    | d `elem` [Fs, FsD, FsR] = "8"
                    | otherwise = error "Unknown duration"

        -- | Get MEI @dots@ from our 'Dur' (omit attribute if duration is not dotted)
        dotsAttr | dur p `elem` [BrD, SbD, MnD, SmD, FsD] = attr "dots" "1"
                 | otherwise = ""

-- | Convert our 'Accid' to MEI @accid@ (omit if natural)
meiAccid :: Pitch -> String
meiAccid p | accid p == Na = ""
           | otherwise = attr "accid" accidString 
    where accidString = ["ff", "f", "n", "s", "ss", "_"] !! (fromEnum $ accid p)

-- *** Conversions for lyrics

-- | If a word is a single syllable, we do not need to include @\@con@ or
-- @\@wordpos@; if it is more than one syllable, we include these attributes.
-- We use a dash connector by default, and get the word position from the data
-- in the 'Syllable' type.
meiSyllable :: Syllable -> String
meiSyllable syl = case sylPosition syl of
    Tacet -> ""
    Only  -> element "syl" [text]
    _     -> elementAttr "syl" 
                [unwords [sylConnector "d", sylPos2mei $ sylPosition syl] ]
                [text]
    where 
        text = sylText syl

        -- | Syllable connector: We call this with default @d@ for dash, but
        -- could be modified to elide syllables (@b@ for breve, curved line
        -- below).
        sylConnector :: String -> String
        sylConnector value = attr "con" value

        -- | Convert our 'SyllablePosition' to MEI @wordpos@ (first/initial,
        -- @i@; middle, @m@; or last/terminal, @t@)
        sylPos2mei :: SyllablePosition -> String
        sylPos2mei pos = attr "wordpos" [posChar]
            where posChar = "imt__" !! fromEnum pos
                -- last two positions are Only and Tacet which shouldn't make
                -- it this far

-- * Write large groups to MEI

-- | Make an XML string containing a list of @note@ elements out of a
-- 'MusicPhrase'
phrase2mei :: MusicPhrase -> String
phrase2mei phrase = (concat $ map note2mei $ notes phrase) ++ element "barLine" []

-- | Make an XML string containing all the contents of one @layer@ out of a
-- 'MusicSentence'
sentence2mei :: MusicSentence -> String
sentence2mei sent = concat $ map phrase2mei sent

-- | A 'MusicSection' contains all the music for one section, /for a single
-- voice/: so combine all subdivisions into one @staff@ and @layer@ so this
-- can be made part of an MEI @section@ in 'chorus2mei'.  
-- Include MEI 1-indexed staff number derived from 'VoiceName' enum 
--
-- __ TODO __ you could put more than one layer per staff if you wanted a
-- 2-staff choirstaff (e.g., SA on one, TB on the other)
section2mei :: MusicSection -> String
section2mei sec = 
    elementAttr "staff"
        [ attr "n" $ show voicenum]
        [ elementAttr "layer" 
            [ attr "n" $ show voicenum ]
            [ sentences ]
        ]
    where 
        voicenum = (fromEnum $ secVoiceID sec) + 1
        sentences = concat $ map sentence2mei $ secSentences sec

-- | Take a list of sections, one per SATB voice, and create a single MEI
-- @section@ including all the voices
chorus2mei :: Arca -> MusicChorus -> String
chorus2mei arca chorus = 
    element "section"
        [ element "scoreDef" 
            [ key
            , meter 
            ]
        , music
        ]
    where 
        config = secConfig $ soprano chorus
        key    = meiKey (arkMode config) (systems arca) 
        meter  = meiMeter $ arkMusicMeter config
        music  = concat $ map section2mei $ chorus2list chorus

-- | Create an MEI key signature (all naturals or one flat) based on mode
meiKey :: Mode -> ModeSystem -> String
meiKey mode modeSystem = elementAttr "keySig" 
                            [ attr "sig" sigString ]
                            []
    where sigString | modeMollis mode modeSystem = "1f" 
                    | otherwise                  = "0"

-- | Create an MEI meter signature (using modern equivalents of Kircher's C,
-- C3, cutC3).
-- __TODO__ I can't figure out how to get this to show up in the Verovio
-- output!
meiMeter :: MusicMeter -> String
meiMeter meter = 
    elementAttr "meterSig" 
        [ attr "count" count 
        , attr "unit" unit 
        ]
        []
    where 
        count = show $ fst meterValues
        unit  = show $ snd meterValues

        meterValues = case meter of
            Duple       -> (4, 2)
            TripleMajor -> (3, 1)
            TripleMinor -> (3, 2)


-- | Extract a simple list of 'MusicSentence' from the four members of a
-- 'MusicChorus'
chorus2list :: MusicChorus -> [MusicSection]
chorus2list chorus = [fn chorus | fn <- [soprano, alto, tenor, bass]]



score2mei :: Arca -> ArkMetadata -> MusicScore -> String
score2mei arca metadata score = meiDocument meiTitle meiPoet meiScore
    where 
        meiTitle = arkTitle metadata
        meiPoet  = arkWordsAuthor metadata
        meiScore = concat $ map (chorus2mei arca) score

-- *** Constants for XML document
_whoami = "Arca musarithmica Athanasii Kircherii MDCL"

_Kircher = "Athanasius Kircher Societatis Iesu"

_AAC = "Andrew A. Cashner, PhD"

_xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

_meiVersion = "4.0.1"

_projectDesc = "This music was generated automatically using Athanasius \
\ Kircher's Arca musarithmica, a device and system he described in 1650 for \
\ generating music by choosing from sets of predefined permutations of pitches \
\ and rhythms. Andrew Cashner created a digital implementation of the ark in the \
\ Haskell programming language in 2021. It takes parsed texts in XML format and \
\ outputs their musical setting in MEI XML encoding." 

-- | Plug in variables and musical content needed to boilerplate MEI document
meiDocument :: String   -- ^ title 
            -> String   -- ^ poet/author of words
            -> String   -- ^ XML string representing the @section>@ elements
            -> String
meiDocument title poet sections = _xmlHeader ++
    elementAttr "mei" 
        [ attr "xmlns" "https://www.music-encoding.org/ns/mei" 
        , attr "meiversion" _meiVersion
        ]
        [ element "meiHead"
            [ element "fileDesc"
                [ element "titleStmt"
                    [ element "title" 
                        [ title ] 
                    , element "composer"  
                        [ elementAttr "persName"
                            [ attr "role" "creator" ]
                            [ _whoami ]
                        ]
                    , element "lyricist" 
                        [ elementAttr "persName"
                            [ attr "role" "lyricist" ]
                            [ poet ]
                        ] 
                    , element "respStmt"
                        [ elementAttr "persName"
                            [ attr "role" "inventor" ]
                            [ _Kircher ]
                        ,  elementAttr "persName"
                            [ attr "role" "text-preparer, programmer" ]
                            [ _AAC ]
                        ]
                    ]
                , element "pubStmt"
                    [ element "date" 
                        [ "Described 1650, implemented 2021" ]
                    , element "pubPlace" 
                        [ "Rochester" ]
                    , element "availability" 
                        [ "Musical output of the ark is in the public domain." ]
                    ]
                , element "sourceDesc"
                    [ element "source"
                        [ element "bibl"
                            [ element "title" 
                                [ "Musurgia universalis" ]
                            , element "author" 
                                [ "Athanasius Kircher" ]
                            , element "imprint"
                                [ element "pubPlace" 
                                    [ "Rome" ]
                                , element "date" 
                                    [ "1650" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , element "encodingDesc"
                [ element "appInfo"
                    [ element "application"
                        [ element "name" 
                            [ "arca" ] 
                        ]
                    ]
                , element "projectDesc"
                    [ element "p" 
                        [ _projectDesc ] 
                    ]
                ]
            ]
        , element "music"
            [ element "body"
                [ element "mdiv"
                    [ element "score" 
                        [ element "scoreDef" 
                            [ element "pgHead"
                                [ elementAttr "rend"
                                    [ attr "valign" "top" 
                                    , attr "halign" "center" 
                                    , attr "fontsize" "150%"
                                    ]
                                    [ title ]
                                , elementAttr "rend"
                                    [ attr "valign" "bottom" 
                                    , attr "halign" "right" 
                                    ]
                                    [ _whoami ]
                                , elementAttr "rend"
                                    [ attr "valign" "bottom" 
                                    , attr "halign" "left" 
                                    ]
                                    [ poet ]
                                ]
                            , elementAttr "staffGrp"
                                [ attr "n"          "1"
                                , attr "bar.thru"   "false"
                                , attr "symbol"     "bracket"
                                ]
                                [ elementAttr "staffDef"
                                    [ attr "n"          "1"
                                    , attr "lines"      "5"
                                    , attr "clef.line"  "2"
                                    , attr "clef.shape" "G"
                                    ]
                                    []
                                , elementAttr "staffDef"
                                    [ attr "n"          "2"
                                    , attr "lines"      "5"
                                    , attr "clef.line"  "2"
                                    , attr "clef.shape" "G"
                                    ]
                                    []
                                , elementAttr "staffDef"
                                    [ attr "n"              "3"
                                    , attr "lines"          "5"
                                    , attr "clef.line"      "2"
                                    , attr "clef.shape"     "G"
                                    , attr "clef.dis"       "8"
                                    , attr "clef.dis.place" "below"
                                    ]
                                    []
                                , elementAttr "staffDef"
                                    [ attr "n"          "4"
                                    , attr "lines"      "5"
                                    , attr "clef.line"  "4"
                                    , attr "clef.shape" "F"
                                    ]
                                    []
                                ]
                            ]
                            , sections 
                        ]
                    ]
                ]
            ]
        ]
