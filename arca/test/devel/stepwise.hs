{- testing improved implementation of 'stepwise' function in Kircher arca
 - Andrew Cashner, 2021/08/30
 -}

import Data.List
import Data.Maybe
import Debug.Trace
   (trace)

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

-- * Test pitches
tooLow :: Range -> Pitch -> Bool
tooLow range p = p7diff p (low range) < 0

tooHigh :: Range -> Pitch -> Bool
tooHigh range p = p7diff p (high range) > 0

inRange :: Range -> Pitch -> Bool
inRange range p = not $ tooLow range p || tooHigh range p

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

-- *** Simple binary tree
-- | Build a binary tree from a list of the options at each level, where there
-- are always one or two options
btree :: a -> [[a]] -> Btree a
btree x []              = Node x Empty Empty
btree x ((y:[]):zs)     = Node x (btree y zs) Empty
btree x ((y1:y2:[]):zs) = Node x (btree y1 zs) (btree y2 zs)
btree _ _               = error "Unknown pattern match error in btree!"

-- | Build a binary tree of values that pass a parent-child test
testBtree :: (a -> a -> Bool) -> a -> [[a]] -> Btree a
testBtree f x []                 = Node x Empty Empty
testBtree f x ((y:[]):zs) 
    | f x y                     = Node x (testBtree f y zs) Empty 
    | otherwise                 = Node x Empty Empty
testBtree f x ((y1:y2:[]):zs) 
    | f x y1 && f x y2          = Node x (testBtree f y1 zs) (testBtree f y2 zs)
    | f x y1 && (not $ f x y2)  = Node x (testBtree f y1 zs) Empty
    | f x y2 && (not $ f x y1)  = Node x (testBtree f y2 zs) Empty
    | otherwise                 = Node x Empty Empty
testBtree f _ _                  = error "Unknown pattern match error in testBtree!"

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

-- * Process a set of pitches

-- ** From list of pitches to decision tree
newPitchSet :: [Char] -> [Pitch]
newPitchSet = map (\p -> Pitch p 0) 

stepwise :: [[Pitch]] -> Btree Pitch
stepwise = testTree legalLeap Nothing

pitchPaths :: [[Pitch]] -> String
pitchPaths ps = intercalate ", " $ map ((intercalate "-") . (map show)) ps

testPitches :: String -- ^ pitch names as string, eg., "ECBA"
            -> String
testPitches pnames = options
    where
        sopranoRange = Range (Pitch 'B' 3) (Pitch 'D' 5)
        music        = pitchCandidates sopranoRange $ newPitchSet pnames
        musicTree    = stepwise music
        allPaths     = paths [] musicTree 
        goodPaths    = fullPaths pnames allPaths
        options      = pitchPaths goodPaths

