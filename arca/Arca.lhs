\documentclass{article}
%include polycode.fmt
\usepackage{parskip}

\title{@arca@: Kircher's \emph{Arca musarithmica} of 1650 in Haskell}
\author{Andrew A. Cashner}

\begin{document}
\maketitle
\tableofcontents

\section{History}
\begin{tabular}{rl}
2019/10/24 & Begun \\
\end{tabular}

\section{Purpose}
Implement Athanasius Kircher's 1650 \emph{Arca musarithmica} for automatically
generating music.

\section{Program}
\begin{code}
module Arca where

data Accid = Fl | Na | Sh 
    deriving (Enum, Show)

data Dur = Lg | Br | Sb | Mn | Sm | Fs 
    deriving (Enum, Show)

data Voice = Soprano | Alto | Tenor | Bass
    deriving (Enum, Show)

data Pitch = Pitch {
        pnum  :: Int,
        oct   :: Int,
        accid :: Accid,
        dur   :: Dur
    } deriving (Show)

vperm :: [[[Int]]]
vperm = 
    [
        [ -- 0
            [8, 8, 7, 8],
            [5, 5, 5, 5],
            [3, 3, 4, 3],
            [1, 1, 5, 1]
        ],
        [ -- 1
            [5, 6, 7, 8],
            [3, 4, 5, 5],
            [1, 1, 2, 3],
            [1, 4, 2, 1]
        ]
    ]

rperm :: [[Dur]]
rperm = 
    [
        [Mn, Sb, Mn, Br], -- 0
        [Sb, Mn, Mn, Br]  -- 1
    ]


-- dummy offsets
knum2pnum :: Int -> Int -> Int
knum2pnum knum mode = knum - 1 + offset !! (mode - 1)
-- 1-indexed knum and mode
    where 
        offset :: [Int]
        offset = [1, 1, 2, 2, 3, 3, 4, 4, 6, 6, 0, 0]

-- dummy octaves
getOctave :: Voice -> Int -> Int
getOctave voice pnum = octaves !! fromEnum voice
    where
        octaves :: [Int]
        octaves = [3, 3, 4, 4]

-- dummy modeAccid
getAccid :: Int -> Int -> Accid
getAccid mode knum = modeAccid !! modeAdjust !! knumAdjust
-- 1-indexed mode and knum
    where 
        modeAdjust = mode - 1

        knumAdjust 
            | knum > 7 = knum - 8 -- reset octave and 0-index
            | otherwise = knum - 1

        modeAccid :: [[Accid]]
        modeAccid =
            [
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh],
                [Na, Na, Na, Na, Na, Fl, Sh]
            ]


perm2pitch :: [[[Int]]] -> Int -> Voice 
    -> [[Dur]] -> Int -> Int 
    -> Int -> Pitch
perm2pitch vperm vIndex voice rperm rIndex noteIndex mode =
    Pitch { 
        pnum  = thispnum,
        oct   = getOctave voice thispnum,
        accid = getAccid mode knum,
        dur   = rperm !! rIndex !! noteIndex
    } where 
        knum = vperm !! vIndex !! fromEnum voice !! noteIndex
        thispnum = knum2pnum knum mode
-- TODO adjust octave when pitch is > 6 or < 0


\end{code}
\end{document}
