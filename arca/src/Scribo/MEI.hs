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
                    | d == LgR                = "long"
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
--
-- __TODO__ Verovio will display the accidental if the @accid@ attribute or
-- element is present, regardless of the key signature. We would need to check
-- the mode/key and then use accid.ges for accidentals that are in the key
-- signature.
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

-- | Where is this item in the list that contains it?
data ListPosition =   ListHead -- ^ head of list
                    | ListBody -- ^ neither head nor last
                    | ListEnd  -- ^ last item in list
    deriving (Enum, Show, Eq)

-- | Given a function that takes a ListPosition argument and a list, apply the
-- function to the list so that the head is marked as ListListHead, the last
-- as ListEnd, and the middle as ListBody.
positionMap :: (ListPosition -> a1 -> [a2]) -> [a1] -> [[a2]]
positionMap fn ls = [ fn ListHead $ head ls
                    , concat $ map (fn ListBody) $ (tail . init) ls
                    , fn ListEnd $ last ls
                    ]

-- | Make an XML string containing a list of @note@ elements out of a
-- 'MusicPhrase'; end each phrase with @barline@, except for last in the list.
--
-- Leave the barline of the last phrase up to the next-higher function (end of
-- sentence gets regular bar; end of sentence, double bar; end of
-- section, final bar).
phrase2mei :: ListPosition -> MusicPhrase -> String
phrase2mei position phrase 
    | position == ListEnd  = meiNotes
    | otherwise            = meiNotes ++ meiBarline ""
    where 
        meiNotes = concat $ map note2mei $ notes phrase


-- | Make an XML string containing all the contents of one @layer@ out of a
-- 'MusicSentence'.
-- If this is the last sentence in the section, omit the bar so the higher
-- function calling this one can add it.
-- Sentence ends with regular barline.
sentence2mei :: ListPosition -> MusicSentence -> String
sentence2mei position sent 
    | position == ListEnd  = meiPhrases
    | otherwise            = meiPhrases ++ meiBarline ""
    where 
        meiPhrases = unwords $ positionMap phrase2mei sent

-- | A 'MusicSection' contains all the music for one section, /for a single
-- voice/: so combine all subdivisions into one @staff@ and @layer@ so this
-- can be made part of an MEI @section@ in 'chorus2mei'.  
-- Include MEI 1-indexed staff number derived from 'VoiceName' enum 
-- Put a double bar at the end of sections and a final bar at the end of the
-- piece.
--
-- __ TODO __ you could put more than one layer per staff if you wanted a
-- 2-staff choirstaff (e.g., SA on one, TB on the other)
section2mei :: ListPosition -> MusicSection -> String
section2mei position sec = 
    elementAttr "staff"
        [ attr "n" $ show voicenum]
        [ elementAttr "layer" 
            [ attr "n" $ show voicenum ]
            [ meiSentencesWithBar ]
        ]
    where 
        voicenum = (fromEnum $ secVoiceID sec) + 1
        meiSentencesWithBar
            | position == ListEnd = meiSentences ++ meiFinalBar
            | otherwise           = meiSentences ++ meiDoubleBar
        meiSentences = unwords $ positionMap sentence2mei sentences
        sentences    = secSentences sec         
      
-- | Take a list of sections, one per SATB voice, and create a single MEI
-- @section@ including all the voices.
-- Add a final bar at the end.
--
-- __TODO__ the scoreDef does not have any effect in Verovio 
chorus2mei :: Arca -> ListPosition -> MusicChorus -> String
chorus2mei arca position chorus = 
    element "section"
        [ elementAttr "scoreDef" 
            [ meter
            , key 
            ]
            []
        , music
        ]
    where 
        config = secConfig $ soprano chorus
        meter  = meiMeterMensural $ arkMusicMeter config
        key    = meiKey (arkMode config) $ systems arca

        music    = concat $ map (section2mei position) choruses
        choruses = chorus2list chorus

-- | Create an MEI key signature (all naturals or one flat) based on mode
-- (@key.sig@ attribute for use in @scoreDef@/@staffDef@)
meiKey :: Mode -> ModeSystem -> String
meiKey mode modeSystem = attr "key.sig" sigString
    where sigString | modeMollis mode modeSystem = "1f" 
                    | otherwise                  = "0"

-- | Create an MEI meter signature (using modern equivalents of Kircher's C,
-- C3, cutC3).
-- (@meterSig with @meter.count@ and @meter.unit@ attributes for use in
-- @scoreDef@/@staffDef@) 
--
-- __TODO__ I can't figure out how to get this or key to show up in the
-- Verovio output! when they aren't in initial scoreDef
meiMeter :: MusicMeter -> String
meiMeter meter = elementAttr "meterSig"
                    [ attr "meter.count" count 
                    , attr "meter.unit" unit 
                    ]
                    []
    where 
        count = show $ fst meterValues
        unit  = show $ snd meterValues

        meterValues = case meter of
            Duple       -> (4, 2)
            TripleMajor -> (3, 1)
            TripleMinor -> (3, 2)

-- | Mensural version of 'meiMeter'
meiMeterMensural :: MusicMeter -> String
meiMeterMensural meter = case meter of 
    Duple       -> unwords 
                    [ attr "mensur.sign"   "C"
                    , attr "mensur.tempus" "2"
                    ]
    TripleMajor -> unwords
                    [ attr "mensur.sign"   "C"
                    , attr "mensur.slash"  "1"
                    , attr "mensur.tempus" "2"
                    , attr "proport.num"   "3"
                    -- , attr "proport.numbase" "1"
                    ]
    TripleMinor -> unwords 
                    [ attr "mensur.sign"   "C"
                    , attr "mensur.tempus" "2"
                    , attr "proport.num"   "3"
                       -- , attr "numbase" "2"
                    ]


-- | Extract a simple list of 'MusicSentence' from the four members of a
-- 'MusicChorus'
chorus2list :: MusicChorus -> [MusicSection]
chorus2list chorus = [fn chorus | fn <- [soprano, alto, tenor, bass]]


-- * Write a whole score to MEI

-- | Convert a whole 'MusicScore' to MEI XML.
-- Include meter and key of first section in top-level @scoreDef@ (__TODO__ ?)
-- Pass on the position in the list to the next function down.
score2mei :: Arca -> ArkMetadata -> MusicScore -> String
score2mei arca metadata score = meiDocument title poet meiScore meter key
    where 
        title    = arkTitle metadata
        poet     = arkWordsAuthor metadata
        meiScore = unwords $ positionMap (chorus2mei arca) score

        config   = secConfig $ soprano $ head score
        meter    = meiMeterMensural $ arkMusicMeter config
        key      = meiKey (arkMode config) $ systems arca

-- ** The full MEI document

-- *** MEI Barlines

-- | Make an MEI double barline
meiDoubleBar :: String
meiDoubleBar = meiBarline "dbl"

-- | Make an MEI final barline
meiFinalBar :: String
meiFinalBar = meiBarline "end"

-- | Make an MEI @barLine@ element
meiBarline :: String -- ^ form string ("dbl", "end", or "" for regular barline)
            -> String
meiBarline form | form == "" = element "barLine" []
                | otherwise  = elementAttr "barLine"
                                 [ attr "form" form ]
                                 []

-- *** String constants for XML document

-- | Default XML header
_xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- | MEI version number
_meiVersion = "4.0.1"

-- | The "composer" (that is, the ark itself)
_whoami = "Arca musarithmica Athanasii Kircherii MDCL"

-- | The "inventor"
_Kircher = "Athanasius Kircher Societatis Iesu"

-- | The "programmer" and "text preparer"
_AAC = "Andrew A. Cashner, PhD"

-- | MEI project description text
_projectDesc = "This music was generated automatically using Athanasius \
\ Kircher's Arca musarithmica, a device and system he described in 1650 for \
\ generating music by choosing from sets of predefined permutations of pitches \
\ and rhythms. Andrew Cashner created a digital implementation of the ark in the \
\ Haskell programming language in 2021. It takes parsed texts in XML format and \
\ outputs their musical setting in MEI XML encoding." 


-- *** The template

-- | Plug in variables and musical content needed to boilerplate MEI document
-- in all its baroque verbosity
meiDocument :: String   -- ^ title 
            -> String   -- ^ poet/author of words
            -> String   -- ^ XML string representing the @section@ elements
            -> String   -- ^ XML string with meter attributes of first section 
                        --   for @staffDef@ (__TODO__ ?)
            -> String   -- ^ XML string with key attributes of same section
            -> String
meiDocument title poet sections meter key = _xmlHeader ++
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
                                    , attr "xml:id" "midi.P1"
                                    , meter
                                    , key
                                    ]
                                    [ elementAttr "instrDef"
                                        [ attr "mid.instrname" "Choir_Aahs" ]
                                        []
                                    ]
                                , elementAttr "staffDef"
                                    [ attr "n"          "2"
                                    , attr "lines"      "5"
                                    , attr "clef.line"  "2"
                                    , attr "clef.shape" "G"
                                    , attr "xml:id" "midi.P2"
                                    , meter
                                    , key
                                    ]
                                    [ elementAttr "instrDef"
                                        [ attr "mid.instrname" "Choir_Aahs" ]
                                        []
                                    ]
                                , elementAttr "staffDef"
                                    [ attr "n"              "3"
                                    , attr "lines"          "5"
                                    , attr "clef.line"      "2"
                                    , attr "clef.shape"     "G"
                                    , attr "clef.dis"       "8"
                                    , attr "clef.dis.place" "below"
                                    , attr "xml:id" "midi.P3"
                                    , meter
                                    , key
                                    ]
                                    [ elementAttr "instrDef"
                                        [ attr "mid.instrname" "Choir_Aahs" ]
                                        []
                                    ]
                                , elementAttr "staffDef"
                                    [ attr "n"          "4"
                                    , attr "lines"      "5"
                                    , attr "clef.line"  "4"
                                    , attr "clef.shape" "F"
                                    , attr "xml:id" "midi.P4"
                                    , meter
                                    , key
                                    ]
                                    [ elementAttr "instrDef"
                                        [ attr "mid.instrname" "Choir_Aahs" ]
                                        []
                                    ]
                                ]
                            ]
                            , sections 
                        ]
                    ]
                ]
            ]
        ]
