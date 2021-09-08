{- calculate figured bass intervals 
 - Andrew Cashner, 2021/09/07
 -}

import Data.List
    (findIndices)

import Data.Function
    (on)

-- * Pitch calculations
-- ** Diatonic
absPitch7 :: Pitch -> Int
absPitch7 p = oct p * 7 + (fromEnum $ pnum p)

pitchMath7 :: (Int -> Int -> Int) -> Pitch -> Pitch -> Int
pitchMath7 f = f `on` absPitch7

p7diff :: Pitch -> Pitch -> Int
p7diff = pitchMath7 (-) 

p7diffMod :: Pitch -> Pitch -> Int
p7diffMod p1 p2 = (p1 `p7diff` p2) `rem` 7 

p7interval = p7diffMod

-- | one-indexed
p7interval1 p1 p2 | diff == 0 = 8
                  | diff <  0 = diff - 1
                  | otherwise = diff + 1
    where diff = p1 `p7interval` p2

-- ** Chromatic
absPitch12 :: Pitch -> Int
absPitch12 p = oct p * 12 + pnum12 + accid12
    where
        pnum12 = case (pnum p) of
            PCc -> 0
            PCd -> 2
            PCe -> 4
            PCf -> 5
            PCg -> 7
            PCa -> 9
            PCb -> 11

        accid12 = case (accid p) of
            Fl -> (-1)
            Na -> 0
            Sh -> 1

pitchMath12 :: (Int -> Int -> Int) -> Pitch -> Pitch -> Int
pitchMath12 f = f `on` absPitch12

p12diff :: Pitch -> Pitch -> Int
p12diff = pitchMath12 (-)

p12diffMod :: Pitch -> Pitch -> Int
p12diffMod p1 p2 = (p1 `p12diff` p2) `rem` 12

p12interval = p12diffMod

-- ** Test intervals

-- | zero-indexed
testInterval12 :: Int -> Pitch -> Pitch -> Bool
testInterval12 i p1 p2 = (abs $ p1 `p12interval` p2) == i

-- | one-indexed
testInterval7 :: Int -> Pitch -> Pitch -> Bool
testInterval7 i p1 p2 = (abs $ p1 `p7interval1` p2) == i

isFourth = testInterval7 4
isFifth  = testInterval7 5

isTritone12 = testInterval12 6

isAugFifth12 p1 p2 = testInterval12 8 p1 p2 || testInterval12 4 p1 p2

-- | augmented fourth or diminished fifth (e.g, C vs F#)
isTritone p1 p2  = (isFourth p1 p2 || isFifth p1 p2) 
                    && isTritone12 p1 p2

-- | augmented fifth or diminished fourth (e.g., C vs G#)
isAugFifth p1 p2  = (isFifth p1 p2 && isAugFifth12 p1 p2)
                    || (isFourth p1 p2 && isAugFifth12 p1 p2)



-- * Data structures
-- ** Pitch labels
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

-- ** Accidentals
data Accid = Fl | Na | Sh
    deriving (Enum, Eq, Ord)

instance Show Accid where
    show a = case a of 
        Fl -> "b"
        Na -> ""
        Sh -> "#"

-- ** Pitches
data Pitch = Pitch {
    pnum :: Pnum,
    accid :: Accid,
    oct :: Int
} deriving (Eq, Ord)

instance Show Pitch where
    show p = (show $ pnum p) ++ (show $ accid p) ++ (show $ oct p)

-- ** Modes
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

-- ** Voices
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

-- ** SATB Chorus
data ChorusSATB = ChorusSATB {
    chorusS :: Voice,
    chorusA :: Voice,
    chorusT :: Voice,
    chorusB :: Voice
} 

instance Show ChorusSATB where
    show ch = unlines $ map show voices
        where voices = [f ch | f <- [chorusS, chorusA, chorusT, chorusB]]

-- ** Figured bass
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

-- * Calculate figured bass
figuredBass :: ChorusSATB -> [FigureStack]
figuredBass chorus = figures
    where
        bass        = music $ chorusB chorus
        voices      = map music [f chorus | f <- [chorusS, chorusA, chorusT]]
        intervals   = map (\v -> zipWith (\upper lower -> Figure (p7interval1 upper lower) (accid upper)) v bass) voices
        figures     = zipWith (\(s:a:t:[]) b -> FigureStack s a t b) (pivot3 intervals) bass

pivot3 :: [[a]] -> [[a]]
pivot3 [] = []
pivot3 ((l:[]):(m:[]):(n:[]):[]) = (l:m:n:[]):[]
pivot3 ((l:ls):(m:ms):(n:ns):[]) = (l:m:n:[]):(pivot3 (ls:ms:ns:[]))

-- * Test figured bass
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

-- ** Show results of testing figured bass
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
