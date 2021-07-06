{-|
Module      : Lectio
Description : Read and process input text to be set to music
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module reads (/lectio/, Latin, "I read") and process input text to be set
to music using the ark. 

= Kircher's specification

Kircher expects the user to prepare a text by segmenting it into phrases
according to the poetic meter and prosody.  In his description the texts are
Latin, but he also demonstrates how the machine could be used with Aramaic and
other languages, ideally by Jesuit missionaries.

= Implementation
== XML input

In our implementation we also expect the user to mark the input text by
dividing the syllables with hyphens and marking the long syllables with accent
symbols (@`@, placed before the relevant syllable), for example:

> Lau-`da-te `Do-mi-num `om-nis `ter-rae. Al-le-`lu-ia. A-`men.

This implementation takes input in the form of an XML document, in which the
text is syllabified and accented as just demonstrated, and divided into one or
more sections. In the attributes for each @\<section\>@ element, the user sets
the values we need as input for the ark:

[@textMeter@]:  e.g., @Prose@ or @Adonium@
[@musicMeter@]: @Duple@, @TripleMinor@, or @TripleMajor@
[@style@]:      @Simple@ (= Syntagma I) or @Florid@ (= Syntagma II)
[@mode@]:       e.g., @Mode1@

Within each section the text is divided into one or more line groups (@\<lg\>@)
and lines (@\<l\>@). (These elements are borrowed from TEI.)

=== __TODO__

In Prose meter, Kircher leaves it up to the user to divide the text into
phrases. We are currently using a very simple algorithm to divide the
text into phrase groups within the correct size range. It would be
better to use a more sophisticated algorithm to parse the text into
optimal groups.

== Reading and parsing the input file

The main function is 'prepareInput', which reads and parses the file and produces a list of 'LyricSection's.

This module reads the input file, parses the XML tree to extract the text and
needed parameters for setting the text (within each section), and then
packages the text into its own data structures to pass on to the other parts
of the program (@Cogito@ for processing and @Scribo@ for writing output).

=== Capturing XML data

The text is first grouped into intermediate data structures that closely
reflect the XML structure. Each @\<section\>@ becomes an 'ArkTextSection',
containing a nested list of strings (line groups and lines from XML) and an
'Aedifico.ArkConfig' with the parameters from the XML section attributes. The list of
these is packaged into a single 'ArkInput' structure containing metadata for
the whole document (taken from the XML @\<head\>@), and a list of
'ArkTextSection's.

=== Preparing for musical setting

The module then processes this data and converts it into a list of
'LyricSection's that the other modules will use.  Below are the structures
that are passed on to other modules, from top down.  Each structure contains
the element below it, plus information about it (length, number of syllables,
etc.). To get that information, these structures are created with methods that
calculate the data upfront.

['LyricSection']: group of sentences (from @\<section\>@)

  * also contains an 'Aedifico.ArkConfig' with the text-setting parameters

['LyricSentence']: group of phrases (from @\<lg\>@)

['LyricPhrase']: group of words (from @\<l\>@)

['Verbum']: individual word, broken into syllables

-}

module Lectio where

import Data.Char
    (isSpace)

import Data.List
    (dropWhileEnd)

import Data.List.Split
    (wordsBy)

import Data.Maybe
    (fromJust)

import Text.XML.Light

import Aedifico
    ( ArkConfig (..)
    , TextMeter (..)
    , toStyle
    , toMode
    , toMusicMeter
    , toTextMeter
    , maxSyllables
    , PenultLength (..)
    , ArkConfig
    )

-- * Read input file

-- ** Global settings for input format

-- | The character used to demarcate syllables (default @\'-\'@)
hyphenChar = '-' :: Char

-- | The character used at the beginning of syllables to show long (or
-- accented) syllables (default @\'\`\'@)
accentChar = '`' :: Char

-- ** Storing XML data

-- | Header information
data ArkMetadata = ArkMetadata {
    arkTitle        :: String,
    arkWordsAuthor  :: String
} deriving Show

-- | The input to the ark is an 'ArkConfig' element with mode, style, and
-- meter; and a list of strings, each of which will become a 'LyricSentence'
data ArkInput = ArkInput {
    arkMetadata :: ArkMetadata,
    arkTextSections :: [ArkTextSection]
} deriving Show

-- | A section of input text (from xml section element)
data ArkTextSection = ArkTextSection {
    arkConfig :: ArkConfig,
    arkText   :: [[String]] -- ^ list of @\<lg\@> containing lists of @\<l\>@
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

-- | Strip leading and trailing whitespace from a 'String'
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

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


-- * Hierarchical text groupings by word, phrase, and sentence

-- ** 'Verbum': Single words and syllables

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

-- ** 'LyricPhrase': Multiple words

-- | A 'LyricPhrase' is a group of 'Verbum' items (words): it contains the list of
-- words, the total count of syllables in the phrase, and a marker for the
-- phrase's penultimate syllable length.
data LyricPhrase = LyricPhrase {
    phraseText          :: [Verbum], -- ^ list of words
    phraseSylCount      :: Int,      -- ^ total syllables in all words
    phrasePenultLength  :: SylLen,   -- ^ length of next-to-last syllable 
                                     --     in whole phrase
    phrasePosition      :: Int       -- ^ position in list of phrases
} deriving (Eq, Ord)

instance Show LyricPhrase where
    show phrase = 
        let
            s   = unwords $ map verbumText $ phraseText phrase
            syl = show $ phraseSylCount phrase
            len = show $ phrasePenultLength phrase
            pos = unwords [",pos:", show $ phrasePosition phrase]
        in 
        unwords [s, syl, len, pos]

-- ** 'LyricSentence': Multiple phrases

-- | Each sentence includes the number of phrases therein
type PhrasesInLyricSentence = Int

-- | A list of totals of phrases in a section 
type PhrasesInLyricSection = [PhrasesInLyricSentence]

-- | A 'LyricSentence' is just a list of 'LyricPhrase' items.
data LyricSentence = LyricSentence { 
    phrases         :: [LyricPhrase],
    sentenceLength  :: PhrasesInLyricSentence -- ^ number of phrases
} deriving (Show, Eq, Ord)

-- ** 'LyricSection': Multiple sentences with parameters for text-setting

-- | A 'LyricSection' includes a list of 'LyricSentence's and an 'ArkConfig'.
--
-- Including an 'ArkConfig' structure makes it possible to structure the input
-- text and program the ark to change meters or modes for different sections. 
data LyricSection = LyricSection {
    sectionConfig :: ArkConfig,
    sentences     :: [LyricSentence]
} deriving (Show, Eq, Ord)

-- *** Get phrase lengths for prepared text

-- | Get the number of phrases per sentence for a whole section.
sectionPhraseLengths :: LyricSection -> PhrasesInLyricSection
sectionPhraseLengths section = map (\ s -> sentenceLength s) $ sentences section

-- | Get the phrase lengths for the whole input structure
inputPhraseLengths :: [LyricSection] -> [PhrasesInLyricSection]
inputPhraseLengths sections = map (\ s -> sectionPhraseLengths s) sections


-- ** Methods to read and store textual data into the above structures

-- | Make a 'LyricSentence' from a list of 'LyricPhrase's.
newLyricSentence :: [LyricPhrase] -> LyricSentence
newLyricSentence ls = LyricSentence {
    phrases = map (\ (p,n) -> LyricPhrase {
        phraseText          = phraseText p,
        phraseSylCount      = phraseSylCount p,
        phrasePenultLength  = phrasePenultLength p,
        phrasePosition      = n
    }) $ zip ls [0,1..],
    sentenceLength  = length ls
}

-- | Take a simple list of 'Verbum' items and make a 'LyricPhrase' structure from
-- it: the original list is stored as 'phraseText', and the 'phraseSylCount'
-- and 'phrasePenultLength' are calculated from that list.
-- The 'phraseSylCount' is the sum of all the 'sylCount's of the words in the
-- list. The 'phrasePenultLength' is the 'penultLength' of the last list item.
newLyricPhrase :: [Verbum] -> LyricPhrase
newLyricPhrase ls = LyricPhrase {
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

-- ** Grouping prose

-- | Regroup a phrase int groups of words with total syllable count in each
-- group not to exceed a given maximum.
--
-- __TODO__: Replace with more sophisticated algorithm:
--      - what to do if word is longer than maxSyllables? (break it into
--      parts?)
--      - optimize this for best grouping, not just most convenient in-order
--
rephrase :: Int     -- ^ maximum syllable count per group
        -> LyricPhrase   -- ^ text already parsed into a 'LyricPhrase'
        -> [LyricPhrase]  -- ^ old phrase broken into list of phrases
rephrase max p = map newLyricPhrase (innerRephrase (phraseText p) []) 
    where
        innerRephrase :: [Verbum] -> [Verbum] -> [[Verbum]]
        innerRephrase [] new = [reverse new]
        innerRephrase old new = 
            let next = (head old) : new in
            if (sum $ map sylCount next) <= max 
                then innerRephrase (tail old) next 
                else (reverse new):(innerRephrase old [])

-- * Read the whole text 

-- | Prepare the entire input structure
prepareInput :: ArkInput -> [LyricSection]
prepareInput input = map (\ s -> prepareLyricSection s) $ arkTextSections input
    where
        -- | Prepare the text of a whole input section
        prepareLyricSection :: ArkTextSection -> LyricSection
        prepareLyricSection sec = LyricSection {
            sectionConfig = config,
            sentences = prepareText meter text
        } where 
            text    = arkText sec
            config  = arkConfig sec
            meter   = arkTextMeter config


        -- | For each string in a list of list of strings: Prepare the string
        -- by converting to a 'LyricSentence' with 'ArkConfig' settings:
        --
        -- Read and parse the string into a 'LyricSentence' of 'LyricPhrase'
        -- elements, each made up of 'Verbum' elements: First 'parse' the
        -- text, then 'rephrase' it for 'maxSyllables'.
        --
        -- | Each @\<lg\>@ element becomes a 'LyricSentence' and @\<l\>@ element
        -- becomes a 'LyricPhrase'.
        prepareText :: TextMeter -> [[String]] -> [LyricSentence]
        prepareText meter text =  
            map (\lg -> newLyricSentence $
                concat $ map (\l -> rephrase (maxSyllables meter) $ parse l) lg) text
        
        -- | Read a string and analyze it into a list of 'Verbum' objects containing
        -- needed information for text setting (syllable count, penult length), using
        -- 'newLyricPhrase'
        parse :: String -> LyricPhrase 
        parse text = newLyricPhrase $ map newVerbum $ words text


