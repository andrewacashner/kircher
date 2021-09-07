{- calculate figured bass intervals 
 -}

import Data.List
    (findIndices)

import Data.Function
    (on)

pitchMath7 :: (Int -> Int -> Int) -> Pitch -> Pitch -> Int
pitchMath7 f = f `on` absPitch7

absPitch7 :: Pitch -> Int
absPitch7 p = oct p * 7 + (fromEnum $ pnum p)

p7diff :: Pitch -> Pitch -> Int
p7diff p1 p2 = pitchMath7 (-) p1 p2

p7diffMod :: Pitch -> Pitch -> Int
p7diffMod p1 p2 = (p1 `p7diff` p2) `mod` 7 

interval1 p1 p2 | diff == 0 = 8
                | otherwise = diff + 1
    where diff = p1 `p7diffMod` p2

data Pnum = PCc | PCd | PCe | PCf | PCg | PCa | PCb
    deriving (Enum, Eq, Ord)

instance Show Pnum where
    show p = case p of
        PCc -> "C"
        PCd -> "D"
        PCe -> "E"
        PCf -> "F"
        PCg -> "G"
        PCa -> "A"
        PCb -> "B"

data Accid = Fl | Na | Sh
    deriving (Enum, Eq, Ord)

instance Show Accid where
    show a = case a of 
        Fl -> "b"
        Na -> ""
        Sh -> "#"

data Pitch = Pitch {
    pnum :: Pnum,
    accid :: Accid,
    oct :: Int
} deriving (Eq, Ord)

instance Show Pitch where
    show p = (show $ pnum p) ++ (show $ accid p) ++ (show $ oct p)

data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Eq, Ord)

instance Show VoiceName where
    show v = case v of
        Soprano -> "Soprano  "
        Alto    -> "Alto     "
        Tenor   -> "Tenor    "
        Bass    -> "Bass     "

data Voice = Voice {
    voiceID :: VoiceName,
    music   :: [Pitch]
} deriving (Eq, Ord)

instance Show Voice where
    show v = (show $ voiceID v) ++ ": " ++ (show $ music v)

data ChorusSATB = ChorusSATB {
    chorusS :: Voice,
    chorusA :: Voice,
    chorusT :: Voice,
    chorusB :: Voice
} 

instance Show ChorusSATB where
    show ch = unlines $ map show voices
        where voices = [f ch | f <- [chorusS, chorusA, chorusT, chorusB]]

data Figure = Figure {
    intervalRelBass :: Int,
    figAccid :: Accid
} deriving (Eq, Ord)

instance Show Figure where
    show f = (show $ intervalRelBass f) ++ (show $ figAccid f)

data FigureStack = FigureStack {
    figureS :: Figure,
    figureA :: Figure,
    figureT :: Figure,
    figureB :: Pitch
} deriving (Eq, Ord)

instance Show FigureStack where
    show fig = (unlines $ map (\s -> " " ++ show s) 
                $ figIntervals fig)
                ++ (show $ figureB fig)

figIntervals :: FigureStack -> [Figure]
figIntervals fig = [f fig | f <- [figureS, figureA, figureT]]

showFigs :: [FigureStack] -> String
showFigs fs = concat [ unlines $ map (concat . map (\n -> show n ++ "\t")) sat
                     , concat $ map (\p -> show p ++ "\t") b
                     ]
    where 
        sat = [map f fs | f <- [figureS, figureA, figureT]]
        b   = map figureB fs


figuredBass :: ChorusSATB -> [FigureStack]
figuredBass chorus = figures
    where
        bass        = music $ chorusB chorus
        voices      = map music [f chorus | f <- [chorusS, chorusA, chorusT]]
        intervals   = map (\v -> zipWith (\upper lower -> Figure (interval1 upper lower) (accid upper)) v bass) voices
        figures     = zipWith (\(s:a:t:[]) b -> FigureStack s a t b) (pivot3 intervals) bass

pivot3 :: [[a]] -> [[a]]
pivot3 [] = []
pivot3 ((l:[]):(m:[]):(n:[]):[]) = (l:m:n:[]):[]
pivot3 ((l:ls):(m:ms):(n:ns):[]) = (l:m:n:[]):(pivot3 (ls:ms:ns:[]))

markFigureStacks :: (FigureStack -> Bool) -> [FigureStack] -> [Bool]
markFigureStacks test figs = map test figs

allIntervals :: [Int] -> FigureStack -> Bool
allIntervals intervals fig = all (\i -> i `elem` intervals) $ map intervalRelBass $ figIntervals fig

anyIntervals :: [Int] -> FigureStack -> Bool
anyIntervals intervals fig = any (\i -> i `elem` intervals) $ map intervalRelBass $ figIntervals fig

perfectConsonance :: FigureStack -> Bool
perfectConsonance = allIntervals [3, 5, 8] 

imperfectConsonance :: FigureStack -> Bool
imperfectConsonance = allIntervals [3, 6, 8]

-- TODO what about 6/4 chords and others?

consonance :: FigureStack -> Bool
consonance fig = perfectConsonance fig || imperfectConsonance fig

dissonance :: FigureStack -> Bool
dissonance = not . consonance

seventh fig = any (== True) $ map (\set -> allIntervals set fig)
    [ [8, 7, 3]
    , [7, 5, 3]
    , [6, 5, 3]
    , [8, 6, 3]
    , [6, 4, 3]
    , [8, 6, 3]
    , [6, 4, 2]
    ]

testFigAccid :: (Accid -> Bool) -> FigureStack -> Bool
testFigAccid test fig = any test $ map figAccid $ figIntervals fig

addedFlat  = testFigAccid (== Fl) 
addedSharp = testFigAccid (== Sh)
addedFlatAndSharp fig = addedFlat fig && addedSharp fig

showMarkedFigureStacks :: (FigureStack -> Bool) -> [FigureStack] -> String
showMarkedFigureStacks test figs = unlines [showFigs figs, values]
    where 
        values = concat $ map (\b -> if b then "*\t" else "\t") bools
        bools  = markFigureStacks test figs

findFigureStacks :: (FigureStack -> Bool) -> [FigureStack] -> [Int]
findFigureStacks = findIndices 

{- data 
 -}
symphonia = ChorusSATB {
    chorusS = Voice {
        voiceID = Soprano,
        music   = [Pitch PCc Na 5, Pitch PCc Na 5, Pitch PCc Na 5, Pitch PCb Na 4, Pitch PCc Na 5, Pitch PCb Fl 4, Pitch PCa Na 4, Pitch PCa Na 4, Pitch PCg Na 4]
    },
    chorusA = Voice {
        voiceID = Alto,
        music   = [Pitch PCg Na 4, Pitch PCa Na 4, Pitch PCg Na 4, Pitch PCf Na 4, Pitch PCe Na 4, Pitch PCc Na 4, Pitch PCf Na 4, Pitch PCd Na 4, Pitch PCd Na 4]
    },
    chorusT = Voice {
        voiceID = Tenor,
        music   = [Pitch PCe Na 4, Pitch PCf Na 4, Pitch PCd Na 4, Pitch PCd Na 4, Pitch PCg Na 3, Pitch PCg Na 3, Pitch PCc Na 4, Pitch PCc Na 4, Pitch PCb Na 3]
    },
    chorusB = Voice {
        voiceID = Bass,
        music   = [Pitch PCc Na 3, Pitch PCf Na 3, Pitch PCg Na 3, Pitch PCg Na 2, Pitch PCc Na 3, Pitch PCe Na 3, Pitch PCf Na 3, Pitch PCf Sh 3, Pitch PCg Na 3]
    }
}

figures = figuredBass symphonia

{- examples:
 - findFigureStacks perfectConsonance figures
 - putStrLn $ showMarkedFigureStacks seventh figures
 - putStrLn $ showMarkedFigureStacks addedFlat figures
 -}
