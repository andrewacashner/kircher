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
                    | d `elem` [Br, BrD, BrR] = "brevis"
                    | d `elem` [Sb, SbD, SbR] = "semibrevis"
                    | d `elem` [Mn, MnD, MnR] = "minima"
                    | d `elem` [Sm, SmD, SmR] = "semiminima"
                    | d `elem` [Fs, FsD, FsR] = "fusa"
                    | otherwise = error "Unknown duration"

        -- | Get MEI @dots@ from our 'Dur' (omit attribute if duration is not dotted)
        dotsAttr | dur p `elem` [BrD, SbD, MnD, SmD, FsD] = attr "dots" "1"
                 | otherwise = ""

-- | Convert our 'Accid' to MEI @accid@
meiAccid :: Pitch -> String
meiAccid p = attr "accid" accidString 
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

-- | Make a @voice@ out of a list of 'Note' (for now)
-- __TODO__ expand
notes2mei :: [Note] -> String
notes2mei notes = element "voice" $ map note2mei notes
