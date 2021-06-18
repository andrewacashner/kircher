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

import Aedifico
import Lectio
import Cogito


-- * Write XML 

-- ** Utilities

-- | Put a string between two other strings
enbrace :: String   -- ^ start
        -> String   -- ^ middle
        -> String   -- ^ end 
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

-- ** 'Note' becomes @note@

-- | Create an MEI @note@ element from our @Note@ datatype, converting the
-- attributes as needed
note2mei note = elementAttr "note"
            [ attr "pname" meiPname
            , attr "oct"   meiOct
            , attr "dur"   meiDur
            , attr "dots"  meiDots
            , attr "accid" meiAccid
            ]
            [ element "verse"
                [ elementAttr "syl"
                    [ attr "con" "d"
                    , attr "wordpos" meiWordpos
                    ]
                    [ meiSyl ]
                ]
            ]
 
            where
                pitch       = notePitch note
                syllable    = noteSyllable note

                meiPname       = pnum2mei   $ pnum pitch
                meiOct         = oct2mei    $ oct pitch
                meiDur         = dur2mei    $ dur pitch
                meiDots        = dur2dots   $ dur pitch
                meiAccid       = accid2mei  $ accid pitch
                meiWordpos     = sylPos2mei $ sylPosition syllable 
                meiSyl         = sylText syllable

-- ** Conversions for 'Note' members

-- | Convert 'Pnum' to MEI @pname@
pnum2mei :: Pnum -> String
pnum2mei p = [c] where c = "cdefgabc_" !! fromEnum p

-- | Just print the octave number
oct2mei :: Int -> String
oct2mei = show

-- | Convert 'Dur' to MEI @dur@ (base value of duration, ignoring dots)
dur2mei :: Dur -> String
dur2mei d | d == DurNil             = "_"
          | d `elem` [Br, BrD, BrR] = "breve"
          | d `elem` [Sb, SbD, SbR] = "1"
          | d `elem` [Mn, MnD, MnR] = "2"
          | d `elem` [Sm, SmD, SmR] = "4"
          | d `elem` [Fs, FsD, FsR] = "8"
          | otherwise = error "Unknown duration"

-- | Get MEI @dots@ from our 'Dur' 
dur2dots :: Dur -> String
dur2dots d | d `elem` [BrD, SbD, MnD, SmD, FsD] = "1"
           | otherwise = "0"

-- | Convert our 'Accid' to MEI @accid@
accid2mei :: Accid -> String
accid2mei a = ["ff", "f", "n", "s", "ss", "_"] !! fromEnum a

-- | Convert our 'SyllablePosition' to MEI @wordpos@:
-- Use @"m"@ for Middle and Only (?)
-- TODO is it okay to use "m" (medial) for individual syllables not part of a
-- larger word?
sylPos2mei :: SyllablePosition -> String
sylPos2mei p | p == First              = "i"
             | p == Last               = "t"
             | p `elem` [Middle, Only] = "m"
             | otherwise               = "_"

-- * Write large groups to MEI

-- | Make a @voice@ out of a list of 'Note' (for now)
-- __TODO__ expand
notes2mei :: [Note] -> String
notes2mei notes = element "voice" $ map note2mei notes
