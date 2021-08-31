{- testing improved implementation of 'stepwise' function in Kircher arca
 - Andrew Cashner, 2021/08/30
 -}

import Data.List
-- import Debug.Trace
--    (trace)

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

p7inc :: Pitch -> Int -> Pitch
p7inc p n = p7pitch $ n + absPitch p 

p7diff :: Pitch -> Pitch -> Int
p7diff p1 p2 | p1 == p2  = 0
             | otherwise = absPitch p1 - absPitch p2

tooLow :: Range -> Pitch -> Bool
tooLow range p = p7diff p (low range) < 0

tooHigh :: Range -> Pitch -> Bool
tooHigh range p = p7diff p (high range) > 0

inRange :: Range -> Pitch -> Bool
inRange range p = not $ tooLow range p || tooHigh range p

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

legalLeap :: Pitch -> Pitch -> Bool
legalLeap p1 p2 = diff <= 7 && diff /= 6
    where diff = abs $ p7diff p1 p2


data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

buildTree :: a -> [[a]] -> Tree a
buildTree x []              = Node x Empty Empty
buildTree x ((y:[]):zs)     = Node x (buildTree y zs) Empty
buildTree x ((y1:y2:[]):zs) = Node x (buildTree y1 zs) (buildTree y2 zs)
buildTree _ _               = error "Unknown pattern match error in buildTree!"

testTree :: (a -> a -> Bool) -> a -> [[a]] -> Tree a
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

newPitchSet :: [Char] -> [Pitch]
newPitchSet = map (\p -> Pitch p 0) 

pitchTree :: (Pitch -> Pitch -> Bool) -> [[Pitch]] -> Tree Pitch
pitchTree f ((x:xs):ys) = testTree f x ((x:xs):ys) -- duplicate first pitch as tree root

stepwise :: [[Pitch]] -> Tree Pitch
stepwise = pitchTree legalLeap


main :: IO()
main = do
    notes <- getLine

    let 
        sopranoRange = Range (Pitch 'B' 3) (Pitch 'D' 5)
        music        = pitchCandidates sopranoRange $ newPitchSet notes
        musicTree    = stepwise music

    putStrLn $ show musicTree
