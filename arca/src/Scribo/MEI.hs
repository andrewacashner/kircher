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

This means that the whole arca program is transforming an input XML document
with the text and parameters into and output XML document with the text
matched to music.
-}

module Scribo.MEI where

import Data.List 
    (transpose)

import Aedifico
import Lectio
import Cogito
import Cogito.Musarithmetic


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
xmltagClose name = enbrace "</" "> " name

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

-- ** @Note@ becomes @note@ (pitch with syllable, or rest)

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
                    [ fn pitch | fn <- [meiPname, meiOct, meiDur] ]
                    [ meiAccid pitch
                    , element "verse" [meiSyllable syllable]
                    ]
        
-- *** Conversions for @Note@ members

-- | Convert 'Pnum' to MEI @pname@
meiPname :: Pitch -> String
meiPname p = attr "pname" c
    where c = case pnum p of
           PCc  -> "c"
           PCd  -> "d"
           PCe  -> "e"
           PCf  -> "f"
           PCg  -> "g"
           PCa  -> "a"
           PCb  -> "b"
           PCc8 -> "c"
           _    -> error $ unwords ["Unknown pitch", show $ pnum p]

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
                    | d `elem` [Lg, LgD, LgR] = "long"
                    | d `elem` [Br, BrD, BrR] = "breve"
                    | d `elem` [Sb, SbD, SbR] = "1"
                    | d `elem` [Mn, MnD, MnR] = "2"
                    | d `elem` [Sm, SmD, SmR] = "4"
                    | d `elem` [Fs, FsD, FsR] = "8"
                    | otherwise = error $ unwords ["Unknown duration", show d]

        -- | Get MEI @dots@ from our 'Dur' (omit attribute if duration is not dotted)
        dotsAttr | dur p `elem` [LgD, BrD, SbD, MnD, SmD, FsD] = attr "dots" "1"
                 | otherwise = ""

-- | Convert our 'Accid' to MEI @accid@ (omit if natural)
--
-- __TODO__ Verovio will display the accidental if the @accid@ attribute or
-- element is present, regardless of the key signature. We would need to check
-- the tone/key and then use accid.ges for accidentals that are in the key
-- signature.
meiAccid :: Pitch -> String
meiAccid p = case accidType p of
    None -> ""
    Written -> elementAttr "accid"
                [ attr "accid" accidString ]
                []
    Implicit -> elementAttr "accid"
                [ attr "accid.ges" accidString ]
                []
    Suggested -> elementAttr "accid"
                [ attr "accid" accidString
                , attr "func" "edit"
                ]
                []
    where accidString = case accid p of
            Fl      -> "f"
            Na      -> "n"
            Sh      -> "s"
            _       -> error $ unwords ["unknown accid", show $ accid p]

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
        sylPos2mei pos = attr "wordpos" meiPos
            where meiPos = case pos of
                    First  -> "i"
                    Middle -> "m"
                    Last   -> "t"
                    _ -> error $ unwords ["Unknown syllable position", show pos]
                    -- last two positions are Only and Tacet which shouldn't make
                    -- it this far

-- * Write large groups to MEI

-- | Where is this item in the list that contains it?
data ListPosition =   ListHead -- ^ head of list
                    | ListBody -- ^ neither head nor last
                    | ListEnd  -- ^ last item in list
                    | ListOnly -- ^ only item of a one-item list
    deriving (Enum, Show, Eq)

-- | Given a function that takes a ListPosition argument and a list, apply the
-- function to the list. This allows you to treat the first and last items in
-- the list differently.
positionMap :: ((ListPosition, a1) -> [a2]) -> [a1] -> [a2]
positionMap fn ls = concat $ map fn $ markedEnds ls

-- | Mark a list with the positions of the items: first, body, and last.
-- Output a list of pairs with the 'ListPosition' and the original list item.
markedEnds :: [a] -> [(ListPosition, a)]

markedEnds []       = []
markedEnds (a:[])   = [ (ListOnly, a) ]
markedEnds (a:b:[]) = [ (ListHead, a)
                      , (ListEnd, b)]
markedEnds (a:b:cs) = [ (ListHead, a) ]
                      ++ (map (\x -> (ListBody, x)) $ init (b:cs))
                      ++ [(ListEnd, last cs)]

-- | Make an XML string containing a list of @note@ elements out of a
-- 'MusicPhrase'; end each phrase with @barline@, except for last in the list.
--
-- Leave the barline of the last phrase up to the next-higher function (end of
-- sentence gets regular bar; end of sentence, double bar; end of
-- section, final bar).
phrase2mei :: (ListPosition, MusicPhrase) -> String
phrase2mei (position, phrase) | position `elem` [ListEnd, ListOnly] 
                                    = meiNotes
                              | otherwise
                                    = meiNotes ++ meiBarline ""
    where meiNotes = concat $ map note2mei $ notes phrase


-- | Make an XML string containing all the contents of one @layer@ out of a
-- 'MusicSentence'.
-- If this is the last sentence in the section, omit the bar so the higher
-- function calling this one can add it.
-- Sentence ends with regular barline.
sentence2mei :: (ListPosition, MusicSentence) -> String
sentence2mei (position, sent) | position `elem` [ListEnd, ListOnly] 
                                    = meiPhrases
                              | otherwise
                                    = meiPhrases ++ meiBarline ""
    where meiPhrases = unwords $ map phrase2mei $ markedEnds sent

-- | A 'MusicSection' contains all the music for one section, /for a single
-- voice/: so combine all subdivisions into one @staff@ and @layer@ so this
-- can be made part of an MEI @section@ in 'chorus2mei'.  
-- Include MEI 1-indexed staff number derived from 'VoiceName' enum 
-- Put a double bar at the end of sections and a final bar at the end of the
-- piece.
--
-- __TODO__: you could put more than one layer per staff if you wanted a
-- 2-staff choirstaff (e.g., SA on one, TB on the other)
section2mei :: Arca -> (ListPosition, MusicSection) -> String
section2mei arca (position, sec) = 
    unwords [ scoreDef 
            , elementAttr "staff"
                [ attr "n" $ show voiceNum
                , attr "corresp" $ show voiceName
                ]
                [ tempo -- TODO This is correct MEI but Verovio does not support this
                , elementAttr "layer" 
                    [ attr "n" "1" ] -- just one layer per staff
                    [ meiSentencesWithBar ]
                ]
            ]
    where 
        voiceNum  = 1 + fromEnum voiceName
        voiceName = secVoiceID sec

        scoreDef | position `elem` [ListHead, ListOnly]
                    = ""
                 | otherwise 
                    = element "scoreDef"
                        [ mensur ++ key ]
        
        tempo   = meiMidiTempo meter
        mensur  = meiMeter meter
        meter   = arkMusicMeter config
        key     = meiKey (arkTone config) $ systems arca

        config = secConfig sec

        meiSentencesWithBar | position `elem` [ListEnd, ListOnly] 
                                = meiSentences ++ meiFinalBar
                            | otherwise 
                                = meiSentences ++ meiDoubleBar

        meiSentences = unwords $ map sentence2mei $ markedEnds sentences
        sentences    = secSentences sec         
      
-- | Take a list of sections, one per SATB voice, and create a single MEI
-- @section@ including all the voices.
-- Add a final bar at the end.
chorus2mei :: Arca -> (ListPosition, MusicChorus) -> String
chorus2mei arca (position, chorus) = element "section" [ music ]
    where 
        config   = secConfig $ soprano chorus
        music    = unwords $ map (\c -> section2mei arca (position, c)) choruses
        choruses = chorus2list chorus

-- | Create an MEI key signature (all naturals or one flat) based on tone
-- (@key.sig@ attribute for use in @scoreDef@/@staffDef@)
meiKey :: Tone -> ToneSystem -> String
meiKey tone toneSystem = elementAttr "keySig"
                            [ attr "sig" $ meiKeySigString tone toneSystem ]
                            []

-- | MEI key signature as an attribute (for use in @staffDef@)
meiKeyAttr :: Tone -> ToneSystem -> String
meiKeyAttr tone toneSystem = attr "key.sig" $ meiKeySigString tone toneSystem 

-- | Value for MEI @key.sig@: one flat if tone is /mollis/, no signature
-- otherwise
meiKeySigString :: Tone -> ToneSystem -> String
meiKeySigString tone toneSystem | toneMollis tone toneSystem = "1f"
                                | otherwise                  = "0"


-- | Switch to select which kind of meter to use as an element
meiMeter :: MusicMeter -> String
meiMeter = meiMeterMensural

-- | Switch to select which kind of meter to use as an attribute
meiMeterAttr :: MusicMeter -> String
meiMeterAttr = meiMeterMensuralAttr

-- | Create an MEI meter signature (using modern equivalents of Kircher's C,
-- C3, cutC3).
-- (@meterSig with @meter.count@ and @meter.unit@ attributes for use in
-- @scoreDef@/@staffDef@) 
meiMeterModern :: MusicMeter -> String
meiMeterModern meter = elementAttr "meterSig"
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

-- | Mensural version of 'meiMeter'. We want either "cut C", "C3", or "cutC3".
-- Verovio does not render these correctly as of 2021/07 when using the
-- @proport@ element (or @proport.num@ attribute) for the number, which is the
-- correct encoding. But putting @num@ directly inside @mensur@ works with
-- Verovio.
meiMeterMensural :: MusicMeter -> String
meiMeterMensural meter = elementAttr "mensur" [ mensur ] []
    where
        mensur = unwords $ case meter of 
            Duple       -> meterCutC
            TripleMinor -> meterC3
            TripleMajor -> meterCutC3
       
        meterCutC   = [ imperfectTempus 
                      , allaBreve
                      ]
        meterC3     = [ imperfectTempus
                      , minorProportion
                      ]
        meterCutC3  = [ imperfectTempus
                      , allaBreve
                      , minorProportion
                      ]

        imperfectTempus = unwords [ attr "sign"    "C"
                                  , attr "tempus"  "2"
                                  ]
        minorProportion = attr "num"     "3"
        allaBreve       = attr "slash" "1"

-- | Mensural meter with proportion as a string of attributes (for use in
-- @staffDef@, where the correct encoding also works with Verovio).
meiMeterMensuralAttr :: MusicMeter -> String
meiMeterMensuralAttr meter = unwords $ case meter of
        Duple       -> [ attr "mensur.sign"   "C"
                       , attr "mensur.tempus" "2"
                       , attr "mensur.slash"  "1"
                       ]
        TripleMajor -> [ attr "mensur.sign"   "C" 
                       , attr "mensur.slash"  "1"
                       , attr "mensur.tempus" "2"
                       , attr "proport.num"   "3"
                       ]
        TripleMinor -> [ attr "mensur.sign"   "C" 
                       , attr "mensur.tempus" "2"
                       , attr "proport.num"   "3"
                       ]

-- | MEI @tempo@ element for MIDI speed: quarter-note (semiminim) beats per
-- minute, different tempi for each mensuration
meiMidiTempo :: MusicMeter -> String
meiMidiTempo meter = elementAttr "tempo"
                        [ meiMidiBPM meter ]
                        []

-- | MEI @midi.bpm@ attribute appropriate for each mensuration
-- This is how it should work, but Verovio does not accept a tempo element
-- within staff, so I do not know how to do a tempo change mid-piece. 
meiMidiBPM meter = attr "midi.bpm" $ show bpm
    where bpm = case meter of
            Duple       -> 120
            TripleMinor -> 180
            TripleMajor -> 320


-- | Extract a simple list of 'MusicSentence' from the four members of a
-- 'MusicChorus'
chorus2list :: MusicChorus -> [MusicSection]
chorus2list chorus = [fn chorus | fn <- [soprano, alto, tenor, bass]]


-- * Write a whole score to MEI

-- | Convert a whole 'MusicScore' to MEI XML.
-- Include meter and key of first section in top-level @scoreDef@ (__TODO__ ?)
-- Pass on the position in the list to the next function down.
score2mei :: Arca -> ArkMetadata -> MusicScore -> String
score2mei arca metadata score = meiDocument title poet key meter bpm meiScore 
    where 
        title    = arkTitle metadata
        poet     = arkWordsAuthor metadata
        config   = secConfig $ soprano $ head score
        key      = meiKeyAttr (arkTone config) $ systems arca
        meter    = meiMeterAttr $ arkMusicMeter config
        bpm      = meiMidiBPM $ arkMusicMeter config
        meiScore = unwords $ map (chorus2mei arca) $ markedEnds score

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
_xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> "

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

-- | MEI element for MIDI instrument number (1-indexed)
midiInstrumentNum :: Int -- ^ 1-indexed MIDI instrument number (e.g., 19 = Church Organ)
                  -> String -- ^ MEI @instrDef@ element
midiInstrumentNum n = elementAttr "instrDef" 
                    [ attr "midi.instrnum" $ show n ] 
                    []


-- | MIDI instrument for playback 
_midiInstrument = midiInstrumentNum reedOrgan
    where
        organ      = 19
        reedOrgan  = 20
        trumpet    = 56
        trombone   = 57
        oboe       = 68
        panflute   = 75

-- *** The template

-- | Plug in variables and musical content needed to boilerplate MEI document
-- in all its baroque verbosity
meiDocument :: String   -- ^ title 
            -> String   -- ^ poet/author of words
            -> String   -- ^ XML string with @keySig@ element
            -> String   -- ^ XML string with @mensur@ element (and @proport@ if present)
            -> String   -- ^ XML string with @midi.bpm@ attribute
            -> String   -- ^ XML string representing the @section@ elements
            -> String
meiDocument title poet key meter bpm sections = _xmlHeader ++
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
                        [ elementAttr "scoreDef" 
                            [ bpm ]
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
                                    [ attr "n"              "1"
                                    , attr "xml:id"         "Soprano"
                                    , attr "lines"          "5"
                                    , attr "clef.line"      "2"
                                    , attr "clef.shape"     "G"
                                    , key
                                    , meter
                                    ]
                                    [ _midiInstrument ]
                                , elementAttr "staffDef"
                                    [ attr "n"              "2"
                                    , attr "xml:id"         "Alto"
                                    , attr "lines"          "5"
                                    , attr "clef.line"      "2"
                                    , attr "clef.shape"     "G"
                                    , attr "clef.dis"       "8"
                                    , attr "clef.dis.place" "below"
                                    , key
                                    , meter
                                    ]
                                    [ _midiInstrument ]
                                , elementAttr "staffDef"
                                    [ attr "n"              "3"
                                    , attr "xml:id"         "Tenor"
                                    , attr "lines"          "5"
                                    , attr "clef.line"      "2"
                                    , attr "clef.shape"     "G"
                                    , attr "clef.dis"       "8"
                                    , attr "clef.dis.place" "below"
                                    , key
                                    , meter
                                    ]
                                    [ _midiInstrument ]
                                , elementAttr "staffDef"
                                    [ attr "n"              "4"
                                    , attr "xml:id"         "Bass"
                                    , attr "lines"          "5"
                                    , attr "clef.line"      "4"
                                    , attr "clef.shape"     "F"
                                    , key
                                    , meter
                                    ]
                                    [ _midiInstrument ]
                                ]
                            ]
                            , sections 
                        ]
                    ]
                ]
            ]
        ]
