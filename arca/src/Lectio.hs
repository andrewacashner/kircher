{-|
Module      : Lectio
Description : Read and process input text to be set to music
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module reads (/lectio/, Latin, "I read") and process input text to be set
to music using the ark. 

Kircher expects the user to prepare a text by segmenting it into phrases
according to the poetic meter and prosody.  In his description the texts are
Latin, but he also demonstrates how the machine could be used with Aramaic and
other languages, ideally by Jesuit missionaries.

In our implementation we also expect the user to mark the input text by
dividing the syllables with hyphens and marking the long syllables with accent
symbols (@`@, placed before the relevant syllable).

The user passes the text to the program from standard input.
This would be a valid input text:

> Lau-`da-te `Do-mi-num `om-nis `ter-rae. Al-le-`lu-ia. A-`men.

__TODO__: 
    - Current test system is dropping last syllable of one word. I think thus
    must be related to the grouping, rephrasing.

    - We are currently just reading a string from standard input. We would
    like to read a whole input file but we need to define the format and
    methods for parsing it. In the previous Scheme implementation we used
    XML for this.

    - We are currently using a very simple algorithm to divide the text
    into phrase groups within the correct size range. It would be better to
    use a more sophisticated algorithm to parse the text into optimal groups.
-}

module Lectio where

import Data.List.Split
    (wordsBy)

import Data.Maybe
    (fromJust)

import Data.String.Utils
    (strip)

import Text.XML.Light

import Aedifico
    (ArkConfig (..),
     toStyle,
     toMeter,
     toMode,
     PenultLength (..),
     ArkConfig)

-- * Global settings for input format

-- | The character used to demarcate syllables (default @\'-\'@)
hyphenChar = '-' :: Char

-- | The character used at the beginning of syllables to show long (or
-- accented) syllables (default @\'`\'@)
accentChar = '`' :: Char

-- * Hierarchical text groupings by word, phrase, and sentence

-- | Every syllable is either 'Long' or 'Short'.
type SylLen = Aedifico.PenultLength 

-- | Our data type for a word includes the original text of the word, that
-- text chunked into syllables, the count of those syllables, and a marker of
-- whether the penultimate syllable is short or long.
data Verbum = Verbum {
    verbumText   :: String,     -- ^ original text
    verbumSyl    :: [String],   -- ^ text divided into list of syllables
    sylCount     :: Int,        -- ^ number of syllables
    penultLength :: SylLen      -- ^ length of next-to-last syllable
} deriving (Eq, Ord, Show)

-- | A 'Phrase' is a group of 'Verbum' items (words): it contains the list of
-- words, the total count of syllables in the phrase, and a marker for the
-- phrase's penultimate syllable length.
data Phrase = Phrase {
    phraseText :: [Verbum],         -- ^ list of words
    phraseSylCount :: Int,          -- ^ total syllables in all words
    phrasePenultLength :: SylLen    -- ^ length of next-to-last syllable in whole phrase
} deriving (Eq, Ord)

instance Show Phrase where
    show phrase = 
        let
            s   = unwords $ map verbumText $ phraseText phrase
            syl = show $ phraseSylCount phrase
            len = show $ phrasePenultLength phrase
        in 
        unwords [s, syl, len]

-- | Each sentence includes the number of phrases therein
type PhrasesInSentence = Int

-- | A list of totals of phrases in a section 
type PhrasesInSection = [PhrasesInSentence]

-- | A 'Sentence' is just a list of 'Phrase' items.
data Sentence = Sentence { 
    phrases         :: [Phrase],
    sentenceLength  :: PhrasesInSentence -- ^ number of phrases
} deriving (Eq, Ord)

instance Show Sentence where
    show sentence = unlines $ map show $ phrases sentence

-- | A 'Section' includes a list of 'Sentences' and an 'ArkConfig'.
--
-- Including an 'ArkConfig' structure makes it possible to structure the input
-- text and program the ark to change meters or modes for different sections. 
data Section = Section {
    sentences     :: [Sentence],
    sectionConfig :: ArkConfig
}

-- ** Get phrase lengths for prepared text
-- | Get the number of phrases per sentence for a whole section.
sectionPhraseLengths :: Section -> PhrasesInSection
sectionPhraseLengths section = map (\ s -> sentenceLength s) $ sentences section

-- | Get the phrase lengths for the whole input structure
inputPhraseLengths :: [Section] -> [PhrasesInSection]
inputPhraseLengths sections = map (\ s -> sectionPhraseLengths s) sections


-- ** Methods to read and store textual data into the above structures

-- | Make a 'Sentence' from a list of 'Phrase's.
newSentence :: [Phrase] -> Sentence
newSentence ls = Sentence {
    phrases         = ls,
    sentenceLength  = length ls
}

-- | Take a simple list of 'Verbum' items and make a 'Phrase' structure from
-- it: the original list is stored as 'phraseText', and the 'phraseSylCount'
-- and 'phrasePenultLength' are calculated from that list.
-- The 'phraseSylCount' is the sum of all the 'sylCount's of the words in the
-- list. The 'phrasePenultLength' is the 'penultLength' of the last list item.
newPhrase :: [Verbum] -> Phrase
newPhrase ls = Phrase {
    phraseText = ls,
    phraseSylCount = sum $ map sylCount ls,
    phrasePenultLength = penultLength $ last ls 
}

-- | Take a 'String' and create a 'Verbum' structure:
--
--  - strip the text of diacritics by removing 'hyphenChar' and 'accentChar' characters
--  - extract syllables by stripping accents and splitting at hyphens
--  - get syllable count from list created in previous step
--  - get penultimate syllable length from list of syllables /including/
--  accents, using 'penultValue'
newVerbum :: String -> Verbum
newVerbum s = Verbum {
    verbumText   = plaintext, 
    verbumSyl    = plainSyllables,
    sylCount     = length plainSyllables,
    penultLength = penultValue accentSyllables
} where 
    plaintext = filter (flip notElem [hyphenChar, accentChar]) s -- no accents or hyphens
    noAccents = filter (/= accentChar) s
    accentSyllables = wordsBy (== hyphenChar) s          -- list of syllables including accents
    plainSyllables  = wordsBy (== hyphenChar) noAccents  -- list of syllables without accents

-- *** Helper methods for parsing

-- | Determine the length of the next-to-last in a list of strings.
-- If the list length is 1 or shorter, or if there is no 'accentChar' at the
-- beginning of the penultimate syllable (found using 'penult'), then the
-- result is 'Short'; otherwise 'Long'.
penultValue :: [String] -> SylLen
penultValue text 
    | length text <= 1 = Short
    | head (penult text) /= accentChar = Short
    | otherwise = Long

-- | Return the next-to-last item in a list.
penult :: [a] -> a
penult = head . tail . reverse


-- * Grouping 

-- | Regroup a phrase int groups of words with total syllable count in each
-- group not to exceed a given maximum.
--
-- We copy the 'ArkConfig' from the old 'Sentence' to the new one.
--
-- __TODO__: Replace with more sophisticated algorithm:
--      - what to do if word is longer than maxSyllables? (break it into
--      parts?)
--      - optimize this for best grouping, not just most convenient in-order
--
rephrase :: Int     -- ^ maximum syllable count per group
        -> Phrase   -- ^ text already parsed into a 'Phrase'
        -> Sentence -- ^ rephrased 'Sentence'
rephrase max p = newSentence parsedPhrases 
    where
        parsedPhrases = map newPhrase (innerRephrase (phraseText p) []) 

        innerRephrase :: [Verbum] -> [Verbum] -> [[Verbum]]
        innerRephrase [] new = [reverse new]
        innerRephrase old new = 
            let next = (head old) : new in
            if (sum $ map sylCount next) <= max 
                then innerRephrase (tail old) next 
                else (reverse new):(innerRephrase old [])


-- * Read input file

-- | Header information
data ArkMetadata = ArkMetadata {
    arkTitle        :: String,
    arkWordsAuthor  :: String
} deriving Show

-- | The input to the ark is an 'ArkConfig' element with mode, style, and
-- meter; and a list of strings, each of which will become a 'Sentence'
data ArkInput = ArkInput {
    arkMetadata :: ArkMetadata,
    arkSections :: [ArkSection]
} deriving Show

-- | A section of input text (from xml section element)
data ArkSection = ArkSection {
    arkConfig :: ArkConfig,
    arkText   :: [String]
} deriving Show

-- | Create a 'QName' to search the xml tree
xmlSearch :: String -> QName
xmlSearch s = QName {
    qName   = s,
    qURI    = Nothing,
    qPrefix = Nothing
}

-- | Get the text from a node
xmlNodeText :: String   -- ^ element name
            -> Element  -- ^ the node
            -> String   -- ^ node text
xmlNodeText name tree = strContent $ fromJust element
    where
        element    = findElement searchName tree
        searchName = xmlSearch name

-- | For each string in list, break text into strings at newlines, strip leading and trailing
-- whitespace, remove empty strings, remove newlines
cleanUpText :: [String] -> [String]
cleanUpText ss = map (\ s -> unwords $ filter (not . null) $ map strip $ lines s) ss

-- | Read an XML string and return the data for input to the ark ('ArkInput')
readInput :: String -> ArkInput
readInput s = ArkInput {
            arkMetadata  = ArkMetadata {
                arkTitle        = title, 
                arkWordsAuthor  = author
            },
            arkSections     = sections
        }
        where
            xml       = fromJust $ parseXMLDoc s 

            head      = fromJust $ findElement (xmlSearch "head") xml
            title     = xmlNodeText "title" head
            author    = xmlNodeText "wordsAuthor" head

            xText      = fromJust $ findElement (xmlSearch "text") xml
            xSections  = findChildren (xmlSearch "section") xText
            sections   = map parseSection xSections
             
            parseSection :: Element -> ArkSection
            parseSection xSection = ArkSection {
                arkConfig = sectionConfig,
                arkText   = sectionText
            } where
            
                settings = map (\ s -> fromJust $ findAttr (xmlSearch s) xSection) 
                            ["style", "meter", "mode"]

                sectionConfig = ArkConfig {
                    arkStyle = toStyle $ settings !! 0,
                    arkMeter = toMeter $ settings !! 1,
                    arkMode =  toMode  $ settings !! 2
                }

                paras       = findChildren (xmlSearch "p") xSection
                sectionText = cleanUpText $ map strContent paras

-- * Read the whole text 

-- | Read a string and analyze it into a list of 'Verbum' objects containing
-- needed information for text setting (syllable count, penult length), using
-- 'newPhrase'
parse :: String -> Phrase 
parse text = newPhrase verba 
    where
        verba = map newVerbum $ words text

-- | The maximum syllables we can set with the ark is 6. (__TODO__: always?)
maxSyllables :: Int
maxSyllables = 6 :: Int 

-- | Prepare a single string by converting to a 'Sentence' with 'ArkConfig'
-- settings:
--
-- Read and parse a string into a 'Sentence' of 'Phrase' elements, each made
-- up of 'Verbum' elements: First 'parse' the text, then 'rephrase' it for
-- 'maxSyllables'.
prepareText :: String -> Sentence
prepareText text = rephrase maxSyllables (parse text) 

-- | Prepare the text of a whole input section
prepareSection :: ArkSection -> Section
prepareSection sec = Section {
    sectionConfig = arkConfig sec,
    sentences     = map prepareText $ arkText sec
}

-- | Prepare the entire input structure
prepareInput :: ArkInput -> [Section]
prepareInput input = map (\ s -> prepareSection s) $ arkSections input



