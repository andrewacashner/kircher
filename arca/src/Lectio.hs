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
symbols (apostrophes actually, placed before the relevant syllable).

The user passes the text to the program from standard input.
This would be a valid input text:

> Lau-'da-te 'Do-mi-num 'om-nis 'ter-rae. Al-le-'lu-ia. A-'men.

__TODO__: We are currently using a very simple algorithm to divide the text
into phrase groups within the correct size range. It would be better to use a
more sophisticated algorithm to parse the text into optimal groups.
-}

module Lectio where

import Data.List.Split
import Aedifico (PenultLength(Long, Short))

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
} deriving (Eq, Ord, Show)

-- | A 'Sentence' is just a list of 'Phrase' items.
type Sentence = [Phrase]

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

-- | Read a text and analyze it into a list of 'Verbum' objects containing
-- needed information for text setting (syllable count, penult length), using
-- 'newPhrase'
parse :: String -> Phrase 
parse text = newPhrase verba 
    where
        verba = map newVerbum textWords
        textWords = words $ cleanup text

-- | Take a 'String' and create a 'Verbum' structure by splitting at the
-- syllables (marked by hyphens), and getting the syllable count and
-- penultimate Length using 'syllabify'
-- 
-- __TODO__: Is there duplication between verbumSyl and syllabify?
newVerbum :: String -> Verbum
newVerbum s = Verbum {
    verbumText   = (unaccent . unhyphen) s,
    verbumSyl    = splitOn "-" $ unaccent s,
    sylCount     = fst syllables,
    penultLength = snd syllables
} where 
    syllables = syllabify s

-- *** Helper methods for parsing

-- | Remove a given character from a string
censor :: Char      -- ^ Character to be removed
        -> String   -- ^ String to censor
        -> String
censor c s = filter (/= c) s

-- | Censor accent mark (@\'@)
unaccent s = censor '\'' s

-- | Censor hyphen (@-@)
unhyphen s = censor '-' s

-- | Divide a string into syllables and return information about them, a
-- 2-tuple with the syllable count and the penultimate length.
syllabify :: String          -- ^ string with syllables marked by hyphens
            -> (Int, SylLen) -- ^ syllable count, penultimate syllable length
syllabify s = (count, penult) 
    where
        count = length syllables
        penult = penultValue syllables
        syllables = wordsBy (== '-') s

-- | Determine the length of the next-to-last in a list of strings.
-- If the list length is 1 or shorter, or if there is no accent mark at the
-- beginning of the penultimate syllable (found using 'penult'), then the
-- result is 'Short'; otherwise 'Long'.
penultValue :: [String] -> SylLen
penultValue text 
    | length text <= 1 = Short
    | head (penult text) /= '\'' = Short
    | otherwise = Long

-- | Return the next-to-last item in a list.
penult :: [a] -> a
penult = head . tail . reverse

-- | Ignore comments (@% ...@), special command (lines starting with @#@)
cleanup :: String -> String
cleanup text = unlines cleanText
    where
        cleanText = removeSpecial $ removeEmpties noComments
        noComments = map (\ s -> removeComments s) $ lines text

removeComments :: String -> String
removeComments = takeWhile (/= '%') 

removeEmpties :: [String] -> [String]
removeEmpties = filter $ not . strNull

removeSpecial :: [String] -> [String]
removeSpecial = filter (\ s -> head s /= '#')

strNull :: String -> Bool
strNull s = length s == 0

-- | extract title line (first line of file, @# ...@) if there is one)
takeTitle :: String -> String
takeTitle text =
    if head firstLine == '#' 
        then drop 2 firstLine
        else ""
    where
        firstLine = head $ lines text



-- * Grouping 

-- | Regroup a phrase int groups of words with total syllable count in each
-- group not to exceed `max'.
rephrase :: Int -> Phrase -> Sentence
rephrase max p = map newPhrase (innerRephrase (phraseText p) [])
    where
        innerRephrase :: [Verbum] -> [Verbum] -> [[Verbum]]
        innerRephrase [] new = [reverse new]
        innerRephrase old new = 
            let next = (head old) : new in
            if (sum $ map sylCount next) <= max 
                then innerRephrase (tail old) next 
                else (reverse new):(innerRephrase old [])

-- TODO what to do if word is longer than maxSyllables?
-- should break it into parts
--
-- TODO optimize this for best grouping, not just most convenient in-order

showPhrase :: Phrase -> String
showPhrase phrase = unwords [s, syl, len]
    where 
        s = unwords $ map verbumText $ phraseText phrase
        syl = show $ phraseSylCount phrase
        len = show $ phrasePenultLength phrase

showSentence :: Sentence -> String
showSentence sentence = unlines $ map showPhrase sentence

maxSyllables :: Int
maxSyllables = 6 -- TODO always? 

prepareText :: String -> Sentence
prepareText s = rephrase maxSyllables $ parse s

