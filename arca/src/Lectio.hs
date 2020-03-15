{- |
 - Lectio: Read input text and determine parameters for musical setting
 -}

module Lectio where

import Data.List.Split

-- TODO group elements in Verbum list optimally and then send data to
-- 'compose'

data Verbum = Verbum {
    text :: String,
    sylCount :: Int,
    penultLength :: SylLen
} deriving (Show, Eq, Ord)

data SylLen = Unknown | Short | Long
    deriving (Show, Enum, Eq, Ord)

data WordPos = Start | Mid | Pause | Stop
    deriving (Show, Enum, Eq, Ord)

-- | Read a text and analyze it into a list of [Verbum] objects containing
-- needed information for text setting (syllable count, penult length)
parse :: String -> [Verbum]
parse text = map analyze textWords where
    textWords = words (cleanup text)

analyze :: String -> Verbum
analyze s = Verbum {
    text = unmark s,
    sylCount = fst syllables,
    penultLength = snd syllables
} where 
    syllables = syllabify s

unmark :: String -> String
unmark s = filter (not . (`elem` "\'-")) s

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

-- | Ignore comments ('% ...'), special command (lines starting with '#')
cleanup :: String -> String
cleanup text = unlines cleanText
    where
        cleanText = removeSpecial $ removeEmpties noComments
        noComments = map (\ s -> removeComments s) (lines text)

removeComments :: String -> String
removeComments = takeWhile (/= '%') 

removeEmpties :: [String] -> [String]
removeEmpties = filter (not . strNull)

removeSpecial :: [String] -> [String]
removeSpecial = filter (\ s -> head s /= '#')

strNull :: String -> Bool
strNull s = length s == 0

-- | extract title line (first line of file, '# ...') if there is one)
takeTitle :: String -> String
takeTitle text =
    if head firstLine == '#' 
        then drop 2 firstLine
        else ""
    where
        firstLine = head (lines text)

