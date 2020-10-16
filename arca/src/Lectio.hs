{- |
 - Lectio: Read input text and determine parameters for musical setting
 -}

module Lectio where

import Data.List.Split
import Aedifico

-- TODO group elements in Verbum list optimally and then send data to
-- 'compose'

data Verbum = Verbum {
    verbumText   :: String,
    verbumSyl    :: [String],
    sylCount     :: Int,
    penultLength :: PenultLength
} deriving (Eq, Ord, Show)

type SylLen = PenultLength -- from Arca

data Phrase = Phrase {
    phraseText :: [Verbum],
    phraseSylCount :: Int,
    phrasePenultLength :: SylLen
} deriving (Eq, Ord, Show)

type Sentence = [Phrase]

newPhrase :: [Verbum] -> Phrase
newPhrase ls = Phrase {
    phraseText = ls,
    phraseSylCount = sum $ map sylCount ls,
    phrasePenultLength = penultLength $ last ls 
}

-- | Read a text and analyze it into a list of [Verbum] objects containing
-- needed information for text setting (syllable count, penult length)
parse :: String -> Phrase 
parse text = newPhrase verba 
    where
        verba = map newVerbum textWords
        textWords = words $ cleanup text

newVerbum :: String -> Verbum
newVerbum s = Verbum {
    verbumText   = (unaccent . unhyphen) s,
    verbumSyl    = splitOn "-" $ unaccent s,
    sylCount     = fst syllables,
    penultLength = snd syllables
} where 
    syllables = syllabify s

-- TODO duplication between verbumSyl and syllabify?

censor :: Char -> String -> String
censor c s = filter (/= c) s

unaccent s = censor '\'' s
unhyphen s = censor '-' s

syllabify :: String -> (Int, SylLen)
syllabify s = (count, penult) 
    where
        count = length syllables
        penult = penultValue syllables
        syllables = wordsBy (== '-') s

penultValue :: [String] -> SylLen
penultValue text 
    | length text <= 1 = Short
    | head (penult text) /= '\'' = Short
    | otherwise = Long

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

