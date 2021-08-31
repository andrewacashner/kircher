{- testing improved implementation of 'stepwise' function in Kircher arca
 - Andrew Cashner, 2021/08/30
 -}

import Data.List
-- import Debug.Trace
--    (trace)

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
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)

buildTree :: a -> [[a]] -> BinaryTree a
buildTree x []              = Node x Empty Empty
buildTree x ((y:[]):zs)     = Node x (buildTree y zs) Empty
buildTree x ((y1:y2:[]):zs) = Node x (buildTree y1 zs) (buildTree y2 zs)
buildTree _ _               = error "Unknown pattern match error in buildTree!"

testTree :: (a -> a -> Bool) -> a -> [[a]] -> BinaryTree a
testTree f x []                 = Node x Empty Empty
testTree f x ((y:[]):zs) 
    | f x y                     = Node x (testTree f y zs) Empty 
    | otherwise                 = Node x Empty Empty
testTree f x ((y1:y2:[]):zs) 
    | f x y1 && f x y2          = Node x (testTree f y1 zs) (testTree f y2 zs)
    | f x y1 && (not $ f x y2)  = Node x (testTree f y1 zs) Empty
    | f x y2 && (not $ f x y1)  = Node x (testTree f y2 zs) Empty
    | otherwise                 = Node x Empty Empty
testTree f _ _                  = error "Unknown pattern match error in testTree!"

-- ** Left child, right sibling tree
data LcrsTree a = LcrsEmpty | LcrsNode a (LcrsTree a) (LcrsTree a)
    deriving (Show)

lcrsTree :: [[a]] -> LcrsTree a
lcrsTree []          = LcrsEmpty
lcrsTree ((x:[]):[]) = LcrsNode x LcrsEmpty LcrsEmpty           -- no siblings, no children
lcrsTree ((x:[]):ys) = LcrsNode x (lcrsTree ys) LcrsEmpty       -- no siblings, yes children
lcrsTree ((x:xs):[]) = LcrsNode x LcrsEmpty (lcrsTree [xs])     -- no children, yes siblings
lcrsTree ((x:xs):ys) = LcrsNode x (lcrsTree ys) (lcrsTree [xs]) -- both 

lChildren :: LcrsTree a -> [a]
lChildren LcrsEmpty = []
lChildren (LcrsNode x l _) = x:(lChildren l)

lcrsPaths :: LcrsTree a -> [a]
lcrsPaths LcrsEmpty                 = []
lcrsPaths (LcrsNode x LcrsEmpty r) = x:(lcrsPaths r)
lcrsPaths (LcrsNode x l _)         = x:(lcrsPaths l)
-- TODO pass a copy of the tree like a fold?
-- need to go back up the tree to previous good L child and trace next R sibling

-- * Process a set of pitches

-- ** From list of pitches to decision tree
newPitchSet :: [Char] -> [Pitch]
newPitchSet = map (\p -> Pitch p 0) 

pitchTree :: (Pitch -> Pitch -> Bool) -> [[Pitch]] -> BinaryTree Pitch
pitchTree f ((x:xs):ys) = testTree f x ((x:xs):ys) -- duplicate first pitch as tree root

stepwise :: [[Pitch]] -> BinaryTree Pitch
stepwise = pitchTree legalLeap

-- ** From tree to list of pitches
leftChildren :: BinaryTree a -> [a]
leftChildren Empty        = []
leftChildren (Node x l _) = x:(leftChildren l)

rightChildren :: BinaryTree a -> [a]
rightChildren Empty        = []
rightChildren (Node x _ r) = x:(rightChildren r)

allChildren :: BinaryTree a -> [[a]]
allChildren (Node x Empty _)     = []
allChildren (Node x l Empty)     = [x:(leftChildren l), x:(rightChildren l)]
allChildren (Node x l r)         = (allChildren $ Node x l Empty) 
                                    ++ (allChildren $ Node x Empty r)

pitchPaths :: BinaryTree Pitch -> [[Pitch]]
pitchPaths tree = map tail $ allChildren tree

-- * MAIN
main :: IO()
main = do
    notes <- getLine

    let 
        sopranoRange = Range (Pitch 'B' 3) (Pitch 'D' 5)
        music        = pitchCandidates sopranoRange $ newPitchSet notes
        musicTree    = stepwise music
        paths        = pitchPaths musicTree

    putStrLn $ show paths
