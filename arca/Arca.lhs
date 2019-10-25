% vim: set foldmethod=marker :

% {{{1 LaTeX preamble
\documentclass{article}
%include polycode.fmt
\usepackage{parskip}
\usepackage[margin=1in]{geometry}
\usepackage[T1]{fontenc}
\usepackage{newpxtext}
\usepackage{euler}

\title{@arca@: Kircher's \emph{Arca musarithmica} of 1650 in Haskell}
\author{Andrew A. Cashner}

\begin{document}
\maketitle
\tableofcontents
% }}}1

% {{{1 history
\section{History}
\begin{tabular}{rl}
2019/10/24 & Begun \\
\end{tabular}
% }}}1

% {{{1 purpose
\section{Purpose}
Implement Athanasius Kircher's 1650 \emph{Arca musarithmica} for automatically
generating music.
% }}}1

% {{{1 program
% {{{2 module, enums
\section{Module}
\begin{code}
module Arca where
\end{code}

\section{Enumeration Types}
\begin{code}
data Accid = Fl | Na | Sh 
    deriving (Enum, Show)

data Dur = Lg | Br | Sb | Mn | Sm | Fs 
    deriving (Enum, Show)

data Voice = Soprano | Alto | Tenor | Bass
    deriving (Enum, Show)
\end{code}
% }}}2

% {{{2 pitch, vperm, rperm
\section{The @Pitch@ Data Type}
\begin{code}
data Pitch = Pitch {
        pnum  :: Int,
        oct   :: Int,
        accid :: Accid,
        dur   :: Dur
    } deriving (Show)
\end{code}

Adjust a pitch to normal bounds.
If a pitch is given the value |{pnum = 7, oct = 4}|, it should be converted to
|{pnum = 0, oct = 5}|.

\begin{code}
stdPitch :: Pitch -> Pitch
stdPitch (Pitch pnum oct accid dur) =
    if pnum > 6 || pnum < 0
        then 
            let 
                pnumAbsolute = oct * 7 + pnum
                pnumAdjust = quotRem pnumAbsolute 7
                pnum2 = (snd pnumAdjust)
                oct2 = (fst pnumAdjust)
            in Pitch pnum2 oct2 accid dur
        else Pitch pnum oct accid dur
        -- should be identity?

\end{code}
% }}}2

\section{Kircher's Music Tables}
\subsection{Permutations of Voices (Pitch Numbers)}

A @Vperm@ (voice permutation) is a list of phrases, each of which is a list of
voices, each of which is a list of 1-indexed pitch numbers.
These come directly from Kircher.%
\footnote{For now, all this data is dummy values for test purposes.}

\begin{code}
type Vperm = [[[Int]]] 

vpermIndex :: Vperm -> Int -> Voice -> Int -> Int
vpermIndex vperm perm voice note = 
    vperm !! perm !! fromEnum voice !! note

vperm :: Vperm
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
\end{code}

\subsection{Permutations of Rhythms}

Each @Vperm@ selection will be matched with an @Rperm@, a permutation of
rhythmic duration values.
This has one less dimension than @Vperm@ because there is only one set of
rhythms for all four voices.

\begin{code}
type Rperm = [[Dur]] 

rpermIndex :: Rperm -> Int -> Int -> Dur
rpermIndex rperm perm note = rperm !! perm !! note

rperm :: Rperm
rperm = 
    [
        [Mn, Sb, Mn, Br], -- 0
        [Sb, Mn, Mn, Br]  -- 1
    ]
\end{code}
% }}}2

% {{{2 converting
\section{Converting Tables to Music}
\subsection{Pitch Index and Mode}

Convert Kircher's 1-indexed pitch numbers and mode numbers to 0-indexed pitch
numbers in the given mode.

\begin{code}
knum2pnum :: Int -> Int -> Int
knum2pnum knum mode = knum - 1 + offset !! (mode - 1)
-- 1-indexed knum and mode
    where 
        offset :: [Int]
        offset = [1, 1, 2, 2, 3, 3, 4, 4, 6, 6, 0, 0]
\end{code}

\subsection{Pitch Range per Voice}

This is the equivalent of setting up Kircher's \emph{palimpsest phonotacticum}:
that is, choosing a set of clefs and choosing a pitch with octave for a given
pitch-class within the staff range of each clef.%
\footnote{TBD}

\begin{code}
getOctave :: Voice -> Int -> Int
getOctave voice pnum = octaves !! fromEnum voice
    where
        octaves :: [Int]
        octaves = [3, 3, 4, 4]
\end{code}

\subsection{Accidental per Mode ``Scale Degree''}

Adjust the accidental according to the key signature and the mode table.%
\footnote{TBD}
This function starts with 1-indexed mode and pitch num (@knum@).

\begin{code}
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
\end{code}

\subsection{All Together}

Given a choice of @Vperm@ and @Rperm@, return a @Pitch@ for a given voice and
position in the note series.%
\footnote{TODO: adjust octave when pitch is > 6 or < 0}
Standardize the pitch in case the pnum is out of range (i.e., adjust octave as
needed).

\begin{code}
perm2pitch :: Vperm -> Int -> Voice 
    -> Rperm -> Int -> Int 
    -> Int -> Pitch
perm2pitch vperm vIndex voice rperm rIndex noteIndex mode =
    stdPitch Pitch { 
        pnum  = thispnum,
        oct   = getOctave voice thispnum,
        accid = getAccid mode knum,
        dur   = rpermIndex rperm rIndex noteIndex
    } where 
        knum = vpermIndex vperm vIndex voice noteIndex
        thispnum = knum2pnum knum mode
\end{code}
%}}}2
%}}}1

\end{document}
