{- testing improved implementation of 'stepwise' function in Kircher arca
 - Andrew Cashner, 2021/08/30
 -}

import Data.List
import Data.Maybe

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


pnum :: Char -> Maybe Int
pnum p = findIndex (== p) "CDEFGAB"

absPitch :: Pitch -> Int
absPitch p = octave + pc
    where 
        octave = oct p * 7 
        pc  = fromJust $ pnum $ pname p

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
pitchesInRange range p = filter (inRange range) candidates
    where candidates = map (octaveChange p) $ octavesInRange range

pitchCandidates :: Range -> [Pitch] -> [[Pitch]]
pitchCandidates range ps = map (pitchesInRange range) ps


data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

buildTree :: a -> [[a]] -> Tree a
buildTree x []              = Node x Empty Empty
buildTree x ((y:[]):zs)     = Node x (buildTree y zs) Empty
buildTree x ((y1:y2:[]):zs) = Node x (buildTree y1 zs) (buildTree y2 zs)
buildTree _ _               = error "Unknown pattern match error in buildTree!"

testTree :: (a -> a -> Bool) -> a -> [[a]] -> Tree a
testTree f x []          = Node x Empty Empty
testTree f x ((y:[]):zs) | f x y     = Node x (buildTree y zs) Empty 
                         | otherwise = Node x Empty Empty
testTree f x ((y1:y2:[]):zs) 
    | f x y1 && f x y2          = Node x (buildTree y1 zs) (buildTree y2 zs)
    | f x y1 && (not $ f x y2)  = Node x (buildTree y1 zs) Empty
    | f x y2 && (not $ f x y1)  = Node x (buildTree y2 zs) Empty
    | otherwise                 = Node x Empty Empty
testTree f _ _                  = error "Unknown pattern match error in testTree!"

_maxInterval = 5

smallLeap :: Pitch -> Pitch -> Bool
smallLeap p1 p2 = abs (p7diff p1 p2) <= _maxInterval

pitchTree :: (Pitch -> Pitch -> Bool) -> [[Pitch]] -> Tree Pitch
pitchTree f ps = testTree f (head $ head ps) ps

stepwise :: [[Pitch]] -> Tree Pitch
stepwise = pitchTree smallLeap

sopranoRange = Range (Pitch 'B' 3) (Pitch 'D' 5)
notes = map (\p -> Pitch p 0) "CDEGBAD"
music = pitchCandidates sopranoRange notes
musicTree = stepwise music

main :: IO()
main = do
    putStrLn $ show musicTree
