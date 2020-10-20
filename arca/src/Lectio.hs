{-|
Module      : Lectio
Description : Read and process input text to be set to music
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

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
import Aedifico (PenultLength(Long, Short))

-- * Global settings for input format

-- | The character used to demarcate syllables (default @\'-\'@)
hyphenChar = '-' :: Char

-- | The character used at the beginning of syllables to show long (or
-- accented) syllables (default @\'`\'@)
accentChar = '`' :: Char

-- | The character marking the beginning of a comment line (default @\'%\'@)
commentChar = '%' :: Char

-- | The character marking the start of program instructions (default @\'#\'@)
--
-- __TODO__: Details of what these would be TBD
programChar = '#' :: Char

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
            syl = Prelude.show $ phraseSylCount phrase
            len = Prelude.show $ phrasePenultLength phrase
        in 
        unwords [s, syl, len]

-- | A 'Sentence' is just a list of 'Phrase' items.
--
-- __TODO__: This structure should make it possible to structure the input
-- text and program the ark to change meters or modes for different sections. 
data Sentence = Sentence { 
    phrases :: [Phrase] 
} deriving (Eq, Ord)

instance Show Sentence where
    show sentence = unlines $ map Prelude.show $ phrases sentence


-- ** Methods to read and store textual data into the above structures

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

-- | Ignore comments (@% ...@), special command (lines starting with @#@)
cleanup :: String -> String
cleanup text = unlines cleanText
    where
        cleanText = removeSpecial $ removeEmpties noComments
        noComments = map removeComments $ lines text

-- | Remove everything after a 'commentChar' from a string
removeComments :: String -> String
removeComments = takeWhile (/= commentChar) 

-- | Remove empty strings from a list of strings
removeEmpties :: [String] -> [String]
removeEmpties = filter $ not . strNull

-- | Does a string have 0 length? (i.e., is it empty?)
strNull :: String -> Bool
strNull s = length s == 0

-- | Remove strings starting with 'programChar' from list of strings
removeSpecial :: [String] -> [String]
removeSpecial = filter (\ s -> head s /= '#')

-- | Extract title line (first line of file, @# ...@) if there is one)
--
-- __TODO__: Currently unused
takeTitle :: String -> String
takeTitle text =
    if head firstLine == programChar
        then drop 2 firstLine
        else ""
    where
        firstLine = head $ lines text


-- * Grouping 

-- | Regroup a phrase int groups of words with total syllable count in each
-- group not to exceed a given maximum.
--
-- __TODO__: Replace with more sophisticated algorithm:
--      - what to do if word is longer than maxSyllables? (break it into
--      parts?)
--      - optimize this for best grouping, not just most convenient in-order
rephrase :: Int     -- ^ maximum syllable count per group
        -> Phrase   -- ^ text already parsed into a 'Phrase'
        -> Sentence -- ^ rephrased 'Sentence'
rephrase max p = Sentence { 
    phrases = map newPhrase (innerRephrase (phraseText p) []) 
} where
        innerRephrase :: [Verbum] -> [Verbum] -> [[Verbum]]
        innerRephrase [] new = [reverse new]
        innerRephrase old new = 
            let next = (head old) : new in
            if (sum $ map sylCount next) <= max 
                then innerRephrase (tail old) next 
                else (reverse new):(innerRephrase old [])


-- * Read the whole text 

-- | Read a string and analyze it into a list of 'Verbum' objects containing
-- needed information for text setting (syllable count, penult length), using
-- 'newPhrase'
parse :: String -> Phrase 
parse text = newPhrase verba 
    where
        verba = map newVerbum textWords
        textWords = words $ cleanup text

-- | The maximum syllables we can set with the ark is 6. (__TODO__: always?)
maxSyllables :: Int
maxSyllables = 6 :: Int 

-- | Read and parse a string into a 'Sentence' of 'Phrase' elements, each made
-- up of 'Verbum' elements: First 'parse' the text, then 'rephrase' it for
-- 'maxSyllables'.
--
-- __TODO__: Process input file, not just a string
prepareText :: String -> Sentence
prepareText s = rephrase maxSyllables $ parse s

