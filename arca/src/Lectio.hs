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
    textWords = words $ cleanup text

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
        noComments = map (\ s -> removeComments s) $ lines text

removeComments :: String -> String
removeComments = takeWhile (/= '%') 

removeEmpties :: [String] -> [String]
removeEmpties = filter $ not . strNull

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
        firstLine = head $ lines text

data Phrase = Phrase {
    phraseText :: [Verbum],
    phraseSylCount :: Int,
    phrasePenultLength :: SylLen
} deriving (Show, Eq, Ord)

newPhrase :: [Verbum] -> Phrase
newPhrase ls = Phrase {
    phraseText = ls,
    phraseSylCount = sum $ map sylCount ls,
    phrasePenultLength = penultLength $ last ls 
}

showText :: Phrase -> String
showText phrase = unwords $ map text $ phraseText phrase

maxSyllables :: Int
maxSyllables = 5

-- | Group list of @Verbum@ objects into phrases where total of syllables is
-- as large as possible but less than five
--groupWords :: Phrase -> Phrase
--groupWords p = let c = phraseSylCount p in
--    if c <= maxSyllables 
--        then p 
--        else groupWords $ newPhrase $ fst $ splitAt (c `div` 2) $ phraseText p
--
--

-- packBoxes max [] = []
-- packBoxes max [x] = if x <= max then [x] else []
-- packBoxes max (x:xs) 
--     | x >= max = x:(packBoxes max xs)
--     | x + (head xs) >= max = [x, head xs] ++ (packBoxes max (tail xs))
--     | otherwise = [x, (head xs)] ++ (packBoxes max (tail xs)) --
--

consUnderMax :: (Ord a, Num a) => a -> a -> [a] -> [a]
consUnderMax max new ls = 
    if (sum ls + new) <= max 
        then new:ls
        else ls

{-
 - input is an integer for the maximum in each group and a list of integers
 -
 - start a new empty list
 - if the sum of the whole input list <= max, put this as the only element in
 -   list 
 -
 - look at the first element of the input list
 - if it is >= max, make it its group and add it to list of groups
 - if it is < max, keep it and look at the second element
 -
 - if second element >= max, end current group, put second in its own group,
 -   add both to list
 - if first + second = max, put both in one group and add it to list
 - if first + second < max, keep both and look at third element
 -
 -}

packBoxes _ [] = []
packBoxes max (x:xs)
    | sum (x:xs) <= max             = [x:xs]
    | x >= max                      = [x] : (packBoxes max xs)
    | (sum $ take 1 (x:xs)) >= max  = (take 1 (x:xs)) :
                                        (packBoxes max (drop 1 (x:xs)))
    | (sum $ take n (x:xs)) >= max  = (take n (x:xs)) :
                                        (packBoxes max (drop n (x:xs)))
    | otherwise = (packBoxes max (take (n - 1) (x:xs))) :
                    (packBoxes max (drop (n - 1) (x:xs)))

-- trying to copy implementation of splitAt but just don't get it
