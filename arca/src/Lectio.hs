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
     TextMeter (..),
     toStyle,
     toMode,
     toMusicMeter,
     toTextMeter,
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
type SylLen = PenultLength 

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
    phraseText          :: [Verbum], -- ^ list of words
    phraseSylCount      :: Int,      -- ^ total syllables in all words
    phrasePenultLength  :: SylLen,   -- ^ length of next-to-last syllable in whole phrase
    phrasePosition      :: Int       -- ^ position in list of phrases
} deriving (Eq, Ord)

instance Show Phrase where
    show phrase = 
        let
            s   = unwords $ map verbumText $ phraseText phrase
            syl = show $ phraseSylCount phrase
            len = show $ phrasePenultLength phrase
            pos = unwords [",pos:", show $ phrasePosition phrase]
        in 
        unwords [s, syl, len, pos]

-- | Each sentence includes the number of phrases therein
type PhrasesInMusicSentence = Int

-- | A list of totals of phrases in a section 
type PhrasesInMusicSection = [PhrasesInMusicSentence]

-- | A 'MusicSentence' is just a list of 'Phrase' items.
data MusicSentence = MusicSentence { 
    phrases         :: [Phrase],
    sentenceLength  :: PhrasesInMusicSentence -- ^ number of phrases
} deriving (Show, Eq, Ord)

-- instance Show MusicSentence where
--    show sentence = unlines $ map show $ phrases sentence

-- | A 'MusicSection' includes a list of 'MusicSentence's and an 'ArkConfig'.
--
-- Including an 'ArkConfig' structure makes it possible to structure the input
-- text and program the ark to change meters or modes for different sections. 
data MusicSection = MusicSection {
    sectionConfig :: ArkConfig,
    sentences     :: [MusicSentence]
} deriving (Show, Eq, Ord)

-- ** Get phrase lengths for prepared text
-- | Get the number of phrases per sentence for a whole section.
sectionPhraseLengths :: MusicSection -> PhrasesInMusicSection
sectionPhraseLengths section = map (\ s -> sentenceLength s) $ sentences section

-- | Get the phrase lengths for the whole input structure
inputPhraseLengths :: [MusicSection] -> [PhrasesInMusicSection]
inputPhraseLengths sections = map (\ s -> sectionPhraseLengths s) sections


-- ** Methods to read and store textual data into the above structures

-- | Make a 'MusicSentence' from a list of 'Phrase's.
newMusicSentence :: [Phrase] -> MusicSentence
newMusicSentence ls = MusicSentence {
    phrases = map (\ (p,n) -> Phrase {
        phraseText          = phraseText p,
        phraseSylCount      = phraseSylCount p,
        phrasePenultLength  = phrasePenultLength p,
        phrasePosition      = n
    }) $ zip ls [0,1..],
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
    phrasePenultLength = penultLength $ last ls,
    phrasePosition = 0
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
    | head penultWord /= accentChar = Short
    | otherwise = Long
    where penultWord = maybe [] id $ penult text

-- | Return the next-to-last item in a list.
penult :: [a] -> Maybe a
penult ls | null ls    = Nothing
          | otherwise  = Just $ (last . init) ls

-- * Read input file

-- | Header information
data ArkMetadata = ArkMetadata {
    arkTitle        :: String,
    arkWordsAuthor  :: String
} deriving Show

-- | The input to the ark is an 'ArkConfig' element with mode, style, and
-- meter; and a list of strings, each of which will become a 'MusicSentence'
data ArkInput = ArkInput {
    arkMetadata :: ArkMetadata,
    arkTextSections :: [ArkTextSection]
} deriving Show

-- | A section of input text (from xml section element)
data ArkTextSection = ArkTextSection {
    arkConfig :: ArkConfig,
    arkText   :: [[String]] -- list of <lg> containing lists of <l>
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
            arkTextSections     = sections
        }
        where
            xml       = fromJust $ parseXMLDoc s 

            head      = fromJust $ findElement (xmlSearch "head") xml
            title     = xmlNodeText "title" head
            author    = xmlNodeText "wordsAuthor" head

            xText      = fromJust $ findElement (xmlSearch "text") xml
            xSections  = findChildren (xmlSearch "section") xText
            sections   = map parseSection xSections

-- | Parse an XML node tree into a section with configuration and parsed text.
parseSection :: Element -> ArkTextSection
parseSection xSection = ArkTextSection {
    arkConfig = sectionConfig,
    arkText   = getText 
} where

    sectionConfig = ArkConfig {
        arkStyle      = toStyle      $ getSetting xSection "style",
        arkMode       = toMode       $ getSetting xSection "mode",
        arkMusicMeter = toMusicMeter $ getSetting xSection "musicMeter",
        arkTextMeter  = toTextMeter  $ getSetting xSection "textMeter"
    }

    getSetting :: Element -> String -> String
    getSetting tree name = 
        let attr = findAttr (xmlSearch name) tree
        in case attr of
            Nothing -> error "Attribute @" ++ name ++ " not found" 
            Just attr -> attr

    getText = map (\ l -> cleanText l) textLines
        where 
            textLines  = map (\ l -> findChildren (xmlSearch "l") l) lineGroups 
            lineGroups = findChildren (xmlSearch "lg") xSection

    cleanText :: [Element] -> [String]
    cleanText tree = cleanUpText $ map strContent tree


-- * Read the whole text 

-- | Prepare the entire input structure
prepareInput :: ArkInput -> [MusicSection]
prepareInput input = map (\ s -> prepareMusicSection s) $ arkTextSections input
    where
        -- | Prepare the text of a whole input section
        prepareMusicSection :: ArkTextSection -> MusicSection
        prepareMusicSection sec = MusicSection {
            sectionConfig = config,
            sentences = prepareText meter text
        } where 
            text    = arkText sec
            config  = arkConfig sec
            meter   = arkTextMeter config


        -- | For each string in a list of list of strings: Prepare the string
        -- by converting to a 'MusicSentence' with 'ArkConfig' settings:
        --
        -- Read and parse the string into a 'MusicSentence' of 'Phrase' elements, each made
        -- up of 'Verbum' elements: First 'parse' the text, then 'rephrase' it for
        -- 'maxSyllables'.
        --
        -- | Each @<lg>@ element becomes a 'MusicSentence' and @<l>@ element
        -- becomes a 'Phrase'.
        prepareText :: TextMeter -> [[String]] -> [MusicSentence]
        prepareText meter text =  
            map (\lg -> newMusicSentence $
                concat $ map (\l -> rephrase maxSyllables $ parse l) lg) text
            where 
                maxSyllables = case meter of
                    Prose               -> 6
                    Adonium             -> 5
                    Dactylicum          -> 6
                    IambicumEuripidaeum -> 6

        -- | Read a string and analyze it into a list of 'Verbum' objects containing
        -- needed information for text setting (syllable count, penult length), using
        -- 'newPhrase'
        parse :: String -> Phrase 
        parse text = newPhrase $ map newVerbum $ words text

-- * Grouping prose

-- | Regroup a phrase int groups of words with total syllable count in each
-- group not to exceed a given maximum.
--
-- __TODO__: Replace with more sophisticated algorithm:
--      - what to do if word is longer than maxSyllables? (break it into
--      parts?)
--      - optimize this for best grouping, not just most convenient in-order
--
rephrase :: Int     -- ^ maximum syllable count per group
        -> Phrase   -- ^ text already parsed into a 'Phrase'
        -> [Phrase]  -- ^ old phrase broken into list of phrases
rephrase max p = map newPhrase (innerRephrase (phraseText p) []) 
    where
        innerRephrase :: [Verbum] -> [Verbum] -> [[Verbum]]
        innerRephrase [] new = [reverse new]
        innerRephrase old new = 
            let next = (head old) : new in
            if (sum $ map sylCount next) <= max 
                then innerRephrase (tail old) next 
                else (reverse new):(innerRephrase old [])


