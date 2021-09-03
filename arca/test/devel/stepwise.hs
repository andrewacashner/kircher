{- testing improved implementation of 'stepwise' function in Kircher arca
 - Andrew Cashner, 2021/08/30-09/02
 -}

import Data.List
import Data.Maybe

-- * Pitch calculations
data Pitch = Pitch {
    pname :: Char,
    oct   :: Int
} deriving (Eq)

instance Show Pitch where
    show p = pname p:(show $ oct p)

data Range = Range {
    low  :: Pitch,
    high :: Pitch
} deriving (Show)


-- ** Converting from pitch number to name or vice versa
_pnames = "CDEFGAB"

pname2num :: Char -> Int 
pname2num p = let i = findIndex (== p) _pnames
            in case i of 
                Just i  -> i
                Nothing -> error "unknown pitch name"

pnum2name :: Int -> Char
pnum2name n | n >= 0 && n < length _pnames 
            = _pnames !! n
        | otherwise = error "unknown pitch number"

-- ** Change octave (copy pitch, change 'oct' member)
octaveChange :: Pitch -> Int -> Pitch
octaveChange p n = Pitch {
    pname = pname p,
    oct = n
}

octaveInc :: Pitch -> Int -> Pitch
octaveInc p n = octaveChange p (oct p + n)

octaveUp :: Pitch -> Pitch
octaveUp p = octaveInc p 1

octaveDown :: Pitch -> Pitch
octaveDown p = octaveInc p (-1)

-- ** Math with pitches
absPitch :: Pitch -> Int
absPitch p = octave + pc
    where 
        octave = oct p * 7 
        pc  = pname2num $ pname p

p7pitch :: Int -> Pitch
p7pitch n = Pitch {
    pname = pnum2name $ n `mod` 7,
    oct   = n `div` 7
}

p7inc :: Pitch -> Int -> Pitch
p7inc p n = p7pitch $ n + absPitch p 

p7diff :: Pitch -> Pitch -> Int
p7diff p1 p2 | p1 == p2  = 0
             | otherwise = absPitch p1 - absPitch p2

pGt :: Pitch -> Pitch -> Bool
pGt p1 p2 = absPitch p1 > absPitch p2

pLt :: Pitch -> Pitch -> Bool
pLt p1 p2 = absPitch p1 < absPitch p2

-- * Test pitches
tooLow :: Range -> Pitch -> Bool
tooLow range p = pLt p $ low range

tooHigh :: Range -> Pitch -> Bool
tooHigh range p = pGt p $ high range

beyondRange :: Range -> Pitch -> Bool
beyondRange range p = tooLow range p || tooHigh range p

inRange :: Range -> Pitch -> Bool
inRange range p = not $ beyondRange range p

legalLeap :: Pitch -> Pitch -> Bool
legalLeap p1 p2 = diff <= 7 && diff /= 6
    where diff = abs $ p7diff p1 p2

-- * Make lists of pitches
lowestInRange :: Range -> Pitch -> Pitch
lowestInRange range p 
    | inRange range p = p
    | tooLow  range p = lowestInRange range $ octaveUp p
    | otherwise       = lowestInRange range $ Pitch (pname p) (oct $ low range)

octavesInRange :: Range -> [Int]
octavesInRange range = [oct $ low range .. oct $ high range]

pitchesInRange :: Range -> Pitch -> [Pitch]
pitchesInRange range p = filter (inRange expandedRange) candidates
    where 
        candidates    = map (octaveChange p) $ octavesInRange range
        expandedRange = Range {
            low  = low range `p7inc` (-2),
            high = high range `p7inc` 2
        }

pitchCandidates :: Range -> [Pitch] -> [[Pitch]]
pitchCandidates range ps = map (pitchesInRange range) ps

-- * Decision trees
-- ** Binary tree
data Btree a = Empty | Node a (Btree a) (Btree a)
    deriving (Show)

-- *** General tree, implemented as left-child/right-sibling binary tree that
-- can take more than two options at each level

-- | Build a left-child/right-sibling tree from a list of the options at each
-- level, for any number of options
tree :: [[a]] -> Btree a
tree []          = Empty
tree ((x:[]):[]) = Node x Empty Empty                -- no children or siblings
tree ((x:[]):ys) = Node x (tree ys) Empty            -- children but no siblings
tree ((x:xs):[]) = Node x Empty (tree [xs])          -- siblings but no children
tree ((x:xs):ys) = Node x (tree ys) (tree ((xs):ys)) -- both 

-- | Build a left-child/right-sibling tree from a list of the options at each
-- level, only including options that pass a test function; the test function
-- compares each parent to its child. If the value of the parent (previous
-- good value) is 'Nothing' then we know it is the beginning of the tree,
-- there is no previous value to compare.
testTree :: (a -> a -> Bool) -- ^ test to determine if child is valid relative to parent
         -> Maybe a          -- ^ previous value to test
         -> [[a]]            -- ^ list of permutations at each level
         -> Btree a

-- End of line.
testTree f _ [] = Empty

-- No children or siblings: If x is good, make it a final node.
testTree f p ((x:[]):[]) 
    | isNothing p || f (fromJust p) x = Node x Empty Empty 
    | otherwise = Empty

-- Children but no siblings: If x is good, make a node and follow its
-- children, comparing them to x.
testTree f p ((x:[]):ys) 
    | isNothing p || f (fromJust p) x = Node x childTree Empty 
    | otherwise = Empty
    where childTree = testTree f (Just x) ys

-- Siblings but no children: If x is good, make a node and follow its
-- siblings. Compare its siblings to the parent of x.
testTree f p ((x:xs):[]) 
    | isNothing p || f (fromJust p) x = Node x Empty siblingTree 
    | otherwise = siblingTree
    where siblingTree = testTree f p [xs]

-- Both children and siblings: If x is good, make a node and follow both
-- children (compare to x) and siblings. Compare siblings to the parent of x.
testTree f p ((x:xs):ys) 
    | isNothing p || f (fromJust p) x = Node x childTree siblingTree
    | otherwise = siblingTree 
    where 
        childTree   = testTree f (Just x) ys
        siblingTree = testTree f p ((xs):ys)

-- ** Traversal

-- | Traverse a binary tree in preorder.
preorder :: Btree a -> [a]
preorder Empty = []
preorder (Node x l r) = x:(preorder l) ++ (preorder r)

-- | Simple string representation of a binary tree.
preorderString :: Btree Pitch -> String
preorderString Empty = []
preorderString (Node x Empty Empty) = show x ++ ". "
preorderString (Node x l r) = 
    show x ++ "-" ++ (preorderString l)
    ++ "; " ++ preorderString r

-- | Make a list of all good paths in an LCRS tree.
paths :: [[a]] -- ^ accumulator list
      -> Btree a 
      -> [[a]]
paths xs Empty = map reverse xs 
paths [] (Node n l r)               = paths [[n]] l ++ paths [] r
paths ((x:xs):ys) (Node n l Empty)  = paths ((n:x:xs):ys) l
paths ((x:xs):ys) (Node n l r)      = paths ((n:x:xs):ys) l ++ paths ((x:xs):ys) r

-- ** Test the paths
-- | Are all the elements of a list the same length?
sameLengths :: [[a]] -> Bool
sameLengths [] = True
sameLengths (x:xs) = all (== length x) $ map length xs

-- | Prune out paths that are shorter than the original list of items
fullPaths :: [a]   -- ^ list of items to permute
          -> [[b]] -- ^ list of permutations
          -> [[b]]
fullPaths items options = filter (\o -> length o == length items) options

-- *** Score a path for "badness" of different kinds

-- | The ambitus is the widest range of pitches used; the difference between
-- the highest and lowest pitches.
ambitus :: [Pitch] -> Int
ambitus ps = maximum aps - minimum aps
    where aps = map absPitch ps

-- | Calculate and list intervals between pitches in a list. The list will be
-- one item shorter than the list of inputs.
intervals :: [Pitch] -> [Int]
intervals (a:[])    = []
intervals (a:b:[])  = [absInterval a b]
intervals (a:b:cs)  = (absInterval a b):(intervals (b:cs))

-- Take the absolute value of an intervals, the difference between pitches.
absInterval :: Pitch -> Pitch -> Int
absInterval a b = abs $ p7diff a b

-- | Add up all the intervals larger than a fourth (where p7diff > 3 with
-- 0-indexed intervals).
sumBigIntervals :: [Pitch] -> Int
sumBigIntervals = sum . filter (> 3) . intervals

-- | Find all the pitches that exceed a given range, and add up the interval
-- by which they go above or below the limits.
sumBeyondRange :: Range -> [Pitch] -> Int
sumBeyondRange range ps = sum $ map sum [highDegrees, lowDegrees]
    where
        highs = filter (\p -> pGt p $ high range) ps
        lows  = filter (\p -> pLt p $ low range) ps
        highDegrees = map (\p -> absInterval p $ high range) highs
        lowDegrees  = map (\p -> absInterval p $ low range) lows

-- | Calculate weighted "badness" score for a list of pitches. Sum of ambitus,
-- sum of large intervals (x 2), sum of degrees of notes out of range (x 10).
badness :: Range -> [Pitch] -> Int
badness range ps = sum [ ambitus ps
                       , sumBigIntervals ps * 2
                       , sumBeyondRange range ps * 10
                       ]

-- | Choose the path with the lowest "badness"; if there are multiple with the
-- same score, choose the first
bestPath :: Range -> String -> [[Pitch]] -> [Pitch]
bestPath range pnames ps = best
    where 
        full    = fullPaths pnames ps
        ranked  = map (badness range) full
        least   = findIndex (== minimum ranked) ranked
        best    | isNothing least = []
                | otherwise       = full !! fromJust least

-- * Process a set of pitches

-- ** From list of pitches to decision tree

-- | Turn list of pitch names (as string) into list of 'Pitch'es
newPitchSet :: [Char] -> [Pitch]
newPitchSet = map (\p -> Pitch p 0) 

-- | Build a tree of all pitch sequences with appropriate leaps
stepwiseTree :: [[Pitch]] -> Btree Pitch
stepwiseTree = testTree legalLeap Nothing

-- | Print the list of paths
showPathList :: [[Pitch]] -> String
showPathList ps = intercalate ", " $ map showPath ps

showPath :: [Pitch] -> String
showPath = intercalate "-" . map show

-- * Main function

sopranoRange = Range (Pitch 'B' 3) (Pitch 'D' 5)

-- | From list of pitch names (string) to output string of pitches.
testPitches :: Range
            -> String -- ^ pitch names as string, eg., "ECBA"
            -> String
testPitches range pnames = showPath best
    where
        music        = pitchCandidates sopranoRange $ newPitchSet pnames
        musicTree    = stepwiseTree music
        allPaths     = paths [] musicTree 
        best         = bestPath range pnames allPaths

main :: IO()
main = do
    input <- getLine
    let output = testPitches sopranoRange input
    putStrLn output
