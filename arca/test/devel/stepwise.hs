{- testing improved implementation of 'stepwise' function in Kircher arca
 - Andrew Cashner, 2021/08/30
 -}

import Data.List
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


-- | Build a binary tree from a list of the options at each level, where there
-- are always one or two options
btree :: a -> [[a]] -> Btree a
btree x []              = Node x Empty Empty
btree x ((y:[]):zs)     = Node x (btree y zs) Empty
btree x ((y1:y2:[]):zs) = Node x (btree y1 zs) (btree y2 zs)
btree _ _               = error "Unknown pattern match error in btree!"

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

-- | Build a left-child/right-sibling tree from a list of the options at each
-- level, for any number of options
tree :: [[a]] -> Btree a
tree []          = Empty
tree ((x:[]):[]) = Node x Empty Empty           -- no children or siblings
tree ((x:xs):[]) = Node x Empty (tree [xs])     -- siblings but no children
tree ((x:[]):ys) = Node x (tree ys) Empty       -- children but no siblings
tree ((x:xs):ys) = Node x (tree ys) (tree [xs]) -- both 

-- | Build a left-child/right-sibling tree from a list of the options at each
-- level, only including options that pass a test function; the test function
-- compares each parent to its child
testTree :: (a -> a -> Bool) -> [[a]] -> Btree a

testTree f [] = -- trace "\n[end]" 
                Empty

-- siblings but no children
testTree f ((x:xs):[]) = -- trace "\n[sibs but no kids]" 
                            Node x Empty Empty 

-- children but no siblings
testTree f ((x:[]):(y:ys):zs)                         
    | f x y     = -- trace "\n[kids, no sibs; child is good]" 
                    Node x (testTree f ((y:ys):zs)) Empty
    | otherwise = -- trace "\n[kids, no sibs; child is bad]" 
                    testTree f ((x:[]):(ys):zs)

-- both children and siblings
testTree f ((x:xs):(y:ys):zs) 
    | f x y     = -- trace "\n[kids, sibs; child is good]" 
                    Node x (testTree f ((y:ys):zs)) (testTree f ((xs):(y:ys):zs))
    | otherwise = -- trace "\n[kids, sibs; child is bad]" 
                    testTree f ((x:xs):(ys):zs)
    

-- ** Traversal
preorder :: Btree a -> [a]
preorder Empty = []
preorder (Node x l r) = x:(preorder l) ++ (preorder r)

--treePaths :: Btree a -> [[a]] -> [[a]]
--treePaths Empty xs                      = map reverse xs
--treePaths (Node n l r) []               = treePaths l [[n]] ++ treePaths r []
---- treePaths (Node n Empty r) ((x:xs):ys)  = ((n:x:xs):ys) ++ treePaths r ((x:xs):ys)
--treePaths (Node n l r) ((x:xs):ys)      = treePaths l ((n:x:xs):ys) 
--                                            ++ treePaths r ((n:x:xs):ys)

-- each node returns its list of paths?

-- * Process a set of pitches

-- ** From list of pitches to decision tree
newPitchSet :: [Char] -> [Pitch]
newPitchSet = map (\p -> Pitch p 0) 

-- pitchTree :: (Pitch -> Pitch -> Bool) -> [[Pitch]] -> Btree Pitch
-- pitchTree f ((x:xs):ys) = testTree f ((x:xs):ys) -- duplicate first pitch as tree root

stepwise :: [[Pitch]] -> Btree Pitch
stepwise = testTree legalLeap

-- ** From tree to list of full paths
-- pitchPaths :: Btree Pitch -> [[Pitch]]
-- pitchPaths tree = map tail $ allChildren tree

-- * MAIN
main :: IO()
main = do
    notes <- getLine

    let 
        sopranoRange = Range (Pitch 'B' 3) (Pitch 'D' 5)
        music        = pitchCandidates sopranoRange $ newPitchSet notes
        results      = tree music -- stepwise music

    putStrLn $ show results
