{- calculate figured bass intervals 
 -}

import Data.List
    (findIndices)

import Data.Function
    (on)

absPitch7 :: Pitch -> Int
absPitch7 p = oct p * 7 + (fromEnum $ pnum p)

pitchMath7 :: (Int -> Int -> Int) -> Pitch -> Pitch -> Int
pitchMath7 f = f `on` absPitch7

p7diff :: Pitch -> Pitch -> Int
p7diff = pitchMath7 (-) 

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

data Mode = Mode1 | Mode2 | Mode3 | Mode4 | Mode5 | Mode6 | Mode7 | Mode8 | Mode9 | Mode10 | Mode11 | Mode12
    deriving (Enum, Eq)

modalFinal :: Mode -> Pnum
modalFinal m | m `elem` [Mode1,  Mode2]  = PCd
             | m `elem` [Mode3,  Mode4]  = PCe
             | m `elem` [Mode5,  Mode6]  = PCf
             | m `elem` [Mode7,  Mode8]  = PCg
             | m `elem` [Mode9,  Mode10] = PCa
             | m `elem` [Mode11, Mode12] = PCc

scaleDegree1 :: Mode -> Pitch -> Int
scaleDegree1 mode p | diff < 0  = diff + 8
                    | otherwise = diff + 1
    where
        pTest = fromEnum $ pnum p
        pRel  = fromEnum $ modalFinal mode
        diff  = pTest - pRel

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

showUpperFigs :: [FigureStack] -> String
showUpperFigs fs = unlines $ map (concat . map (\n -> show n ++ "\t")) sat
    where sat = [map f fs | f <- [figureS, figureA, figureT]]

showFigs :: [FigureStack] -> String
showFigs fs = concat [ showUpperFigs fs 
                     , concat $ map (\p -> show p ++ "\t") bass
                     ]
    where bass = map figureB fs

showFigsInMode :: Mode -> [FigureStack] -> String
showFigsInMode mode fs = concat [ showUpperFigs fs
                                , concat $ map (\p -> "^" ++ show p ++ "\t") bass
                                ]
    where bass = map (scaleDegree1 mode . figureB) fs


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

showMarkedFigsFn :: ([FigureStack] -> String) -> (FigureStack -> Bool) -> [FigureStack] -> String
showMarkedFigsFn fn test figs = unlines [fn figs, values]
    where 
        values = concat $ map (\b -> if b then "*\t" else "\t") bools
        bools  = map test figs

showMarkedFigs :: (FigureStack -> Bool) -> [FigureStack] -> String
showMarkedFigs = showMarkedFigsFn showFigs 

showMarkedFigsInMode :: Mode -> (FigureStack -> Bool) -> [FigureStack] -> String
showMarkedFigsInMode mode = showMarkedFigsFn (showFigsInMode mode) 

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
 - putStrLn $ showMarkedFigs seventh figures
 - putStrLn $ showMarkedFigs addedFlat figures
 - putStrLn $ showMarkedFigsInMode Mode4 dissonance figures
 -}
