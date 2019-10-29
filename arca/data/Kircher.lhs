% vim: set foldmethod=marker :
\documentclass{haskell}
%include polycode.fmt

\title{@Kircher@: The Data of Kircher's \emph{Arca musarithmica}}
\author{Andrew A. Cashner}

\begin{document}

%{{{1 module
\section{Module, Import}
\begin{code}
module Kircher where
import qualified Data.Vector as V
\end{code}
%}}}1

%{{{1 data types
\section{Data types}

\subsection{Equivalents of Kircher's Rods and Tables}

Duration values
\begin{code}
data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Show)

data Dur = Br | Sb | Mn | Sm | Fs
    | BrD | SbD | MnD | SmD | FsD -- dotted
    | BrR | SbR | MnR | SmR | FsR -- rests
    deriving (Enum, Show)

data Meter = Duple | TripleMajor | TripleMinor
    deriving (Enum, Show)

data Style = Simple | Fugal deriving (Enum)

data PenultLength = Long | Short deriving (Enum)
\end{code}

Elements of the ark
\begin{code}
type Vperm      = [Int]
type VpermChoir = V.Vector(Vperm)
type VpermTable = V.Vector(VpermChoir)

type Rperm      = [Dur]
type RpermMeter = V.Vector(Rperm)
type RpermTable = V.Vector(RpermMeter)

type Column     = (VpermTable, RpermTable)

type Pinax      = V.Vector(Column)
type Syntagma   = V.Vector(Pinax)
type Arca       = V.Vector(Syntagma)
\end{code}

%{{{2 accessing
\subsection{Accessing the Data}
\subsubsection{By index}
Find next layer down for each type.
\begin{code}
syntagma :: Arca -> Int -> Syntagma
syntagma arca i     = arca V.! i

pinax :: Syntagma -> Int -> Pinax
pinax syntagma i    = syntagma V.! i

column :: Pinax -> Int -> Column
column pinax i      = pinax V.! i

vperm :: Column -> Int -> VpermChoir
vperm column i      = (fst column) V.! i

rpermMeter :: Column -> Int -> RpermMeter
rpermMeter column meter = (snd column) V.! meter

rperm :: RpermMeter -> Int -> Rperm
rperm rpermMeter i  = rpermMeter V.! i

voice :: VpermChoir -> Int -> Vperm
voice vperm i       = vperm V.! i
\end{code}

Go straight to a voice and a rhythm permutation, given all the needed variables
and an index (which should be generated randomly). % TODO
First a function to do it with simple numeric indices, then one to use enums
and other meaningful input.

\begin{code}

getVoiceN :: Arca -> Int -> Int -> Int -> Int -> Int -> Vperm
getVoiceN arca s p c v i = voice chorus v
    where
        chorus = vperm (column (pinax (syntagma arca s) p) c) i

getVoice :: Arca -> Style -> PenultLength -> Int -> VoiceName 
    -> Int -> Vperm
getVoice arca style penult sylCount voice i = getVoiceN arca s p c v i
        where
            s = fromEnum style
            p = fromEnum penult
            c = sylCount - 2 -- check that this always works
            v = fromEnum voice

getRpermN :: Arca -> Int -> Int -> Int -> Int -> Int -> Rperm
getRpermN arca s p c m i = rperm meter i 
    where
        meter = rpermMeter (column (pinax (syntagma arca s) p) c) m

getRperm :: Arca -> Style -> PenultLength -> Int -> Meter -> Int -> Rperm
getRperm arca style penult sylCount meter i = getRpermN arca s p c m i
    where
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2
        m = fromEnum meter
\end{code}

%}}}2
%}}}1

%{{{1 arca
\section{The \emph{Arca}}

\begin{itemize}
\item ``Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL.''
\item ``Syntagma I. Melothesias siue Contrapuncti simplicis.''
\item ``Pinax I. Voces Polysyllabae quae penultimam Longam habent.''
\end{itemize}

\begin{code}
arca = V.fromList [syntagma0] :: Arca
    where 
        syntagma0   = V.fromList [pinax0] :: Syntagma

        pinax0      = V.fromList [col0] :: Pinax
        -- , col1, col2, col3, col4, col5, col6, col7, col8]

        col0        = (vperms0, rperms0) :: Column

        vperms0     = V.fromList [v0, v1, v2, v3, v4, v5, 
                        v6, v7, v8, v9] :: VpermTable

        v0 = V.fromList
            [ 
                [5, 5],
                [7, 8],
                [2, 3],
                [5, 1]
            ]

        v1 = V.fromList
            [ 
                [5, 5],
                [7, 7],
                [2, 2],
                [5, 5]
            ]

        v2 = V.fromList
            [
                [5, 5],
                [8, 8],
                [3, 3],
                [8, 8]
            ]

        v3 = V.fromList
            [
                [4, 4],
                [6, 6],
                [8, 8],
                [4, 4]
            ]
        
        v4 = V.fromList
            [
                [4, 3],
                [8, 8],
                [6, 5],
                [4, 8]
            ]

        v5 = V.fromList
            [ 
                [3, 2],
                [8, 7],
                [5, 5],
                [8, 5]
            ]

        v6 = V.fromList
            [
                [5, 5],
                [8, 7],
                [3, 2],
                [8, 5]
            ]

        v7 = V.fromList
            [
                [5, 5],
                [7, 8],
                [2, 3],
                [5, 1] 
            ]

        v8 = V.fromList
            [
                [2, 3],
                [7, 8],
                [5, 5],
                [5, 1]
            ]

        v9 = V.fromList
            [
                [6, 5],
                [8, 8],
                [4, 3],
                [4, 1]
            ] 
            
        rperms0 = V.fromList [r0duple, 
                    r0tripleMajor, r0tripleMinor] :: RpermTable

        r0duple = V.fromList
            [ 
                [Sb, Sb],
                [Mn, Mn],
                [Sm, Sm],
                [Fs, Fs],
                [SbD, Mn],
                [MnD, Sm],
                [SmD, Fs]
            ] 

        r0tripleMajor = V.fromList
            [ 
                [Br, Sb],
                [BrD, BrD]
            ] 

        r0tripleMinor = V.fromList
            [[Sb, Mn]] 

\end{code}
\end{document}
%
%        col1 = 
%            (
%                [ -- vperm
%                     [ -- 0
%                            [3, 2, 3],
%                            [8, 7, 8],
%                            [5, 5, 5],
%                            [1, 5, 1]
%                        ],
%                        [ -- 1
%                            [4, 2, 8],
%                            [8, 7, 6],
%                            [6, 5, 3],
%                            [4, 5, 6]
%                        ],
%                        [ -- 2
%                            [4, 3, 4],
%                            [8, 8, 8],
%                            [6, 5, 5],
%                            [4, 1, 4]
%                        ],
%                        [ -- 3
%                            [2, 2, 2],
%                            [7, 6, 7],
%                            [5, 4, 5],
%                            [5, 2, 5]
%                        ],
%                        [ -- 4
%                            [6, 5, 5],
%                            [8, 7, 7],
%                            [4, 2, 2],
%                            [4, 5, 1]
%                        ],
%                        [ -- 5
%                            [5, 4, 3],
%                            [8, 8, 8],
%                            [3, 6, 5],
%                            [8, 4, 8]
%                        ],
%                        [ -- 6
%                            [5, 5, 5],
%                            [8, 7, 8],
%                            [3, 2, 3],
%                            [1, 5, 1]
%                        ],
%                        [ -- 7
%                            [6, 5, 6],
%                            [8, 8, 8],
%                            [4, 3, 4],
%                            [4, 8, 4]
%                        ],
%                        [ -- 8
%                            [3, 2, 3],
%                            [8, 7, 8],
%                            [5, 5, 5],
%                            [1, 5, 1]
%                        ],
%                        [ -- 9
%                            [6, 5, 5],
%                            [8, 7, 8],
%                            [4, 2, 3],
%                            [4, 5, 1]
%                        ]
%                ],
%                ( -- rperm
%                    [ -- duple
%                        [Sb, Sb, Sb],
%                        [Mn, Sb, Mn],
%                        [SbR, SmR, Sm, Sb, Sb],
%                        [MnR, Mn, SbD, Mn],
%                        [Sb, Mn, Mn],
%                        [SmR, Sm, Sm, Sm],
%                        [Sm, Mn, Sm]
%                    ],
%                    [ -- triple major
%                        [Sb, Sb, Sb],
%                        [SbR, Sb, Br, Sb]
%                    ],
%                    [ -- triple minor
%                        [Mn, Mn, Mn],
%                        [SbR, Mn, Sb, Mn]
%                    ]
%                )
%            )
%\end{code}
%%}}}1
%\end{document}
%
%\endinput
%%{{{1 rest
%% --                    --{{{4 col 2, syl 4
%% --                    [ 
%% --                        [ -- 0
%% --                            [5, 5, 5, 5],
%% --                            [7, 8, 7, 8],
%% --                            [2, 3, 2, 3],
%% --                            [5, 8, 5, 1]
%% --                        ],
%% --                        [ -- 1
%% --                            [3, 3, 2, 3],
%% --                            [8, 8, 7, 8],
%% --                            [5, 5, 5, 5],
%% --                            [8, 8, 5, 1]
%% --                        ],
%% --                        [ -- 2
%% --                            [3, 2, 7, 8],
%% --                            [8, 6, 5, 5],
%% --                            [5, 4, 2, 3],
%% --                            [3, 4, 5, 1]
%% --                        ],
%% --                        [ -- 3
%% --                            [5, 6, 5, 5],
%% --                            [8, 8, 7, 8],
%% --                            [3, 4, 2, 3],
%% --                            [8, 4, 5, 1]
%% --                        ],
%% --                        [ -- 4
%% --                            [6, 5, 6, 5],
%% --                            [8, 8, 8, 8],
%% --                            [4, 3, 4, 3],
%% --                            [4, 8, 4, 8]
%% --                        ],
%% --                        [ -- 5
%% --                            [2, 3, 2, 3],
%% --                            [7, 8, 7, 8],
%% --                            [5, 5, 5, 5],
%% --                            [5, 1, 5, 1]
%% --                        ],
%% --                        [ -- 6
%% --                            [3, 3, 2, 3],
%% --                            [8, 8, 7, 8],
%% --                            [5, 5, 5, 5],
%% --                            [8, 8, 5, 8]
%% --                        ],
%% --                        [ -- 7
%% --                            [3, 2, 7, 8],
%% --                            [8, 6, 5, 5],
%% --                            [5, 4, 2, 3],
%% --                            [3, 4, 5, 1]
%% --                        ],
%% --                        [ -- 8
%% --                            [2, 3, 2, 3],
%% --                            [7, 8, 7, 8],
%% --                            [5, 5, 5, 5],
%% --                            [5, 8, 5, 1]
%% --                        ],
%% --                        [ -- 9
%% --                            [4, 5, 6, 5],
%% --                            [8, 8, 8, 8],
%% --                            [6, 5, 4, 3],
%% --                            [4, 3, 4, 1]
%% --                        ]
%% --                    ],
%% --                    --}}}4
%% --                    --{{{4 col 3, syl 5
%% --                    [ 
%% --                        [ -- 0
%% --                            [2, 3, 3, 2, 3],
%% --                            [7, 8, 8, 7, 8],
%% --                            [5, 5, 5, 5, 5],
%% --                            [5, 3, 1, 5, 1]
%% --                        ],
%% --                        [ -- 1
%% --                             -- XXX cn: last s note in kircher is 5
%% --                            [5, 6, 6, 5, 6], 
%% --                            [7, 8, 8, 8, 8], 
%% --                            [3, 3, 4, 3, 4],
%% --                            [3, 6, 4, 8, 4]
%% --                        ],
%% --                        [ -- 2
%% --                            [3, 4, 3, 2, 3],
%% --                             -- XXX cn: penultimate a note in kircher is 8
%% --                            [8, 8, 8, 7, 8], 
%% --                            [5, 6, 5, 5, 5],
%% --                            [8, 4, 8, 5, 1]
%% --                        ],
%% --                        [ -- 3
%% --                            [2, 8, 2, 7, 8],
%% --                            [6, 5, 6, 5, 5],
%% --                            [4, 3, 2, 2, 3],
%% --                            [2, 3, 4, 5, 1]
%% --                        ],
%% --                        [ -- 4
%% --                            [5, 6, 5, 4, 3],
%% --                            [8, 8, 8, 8, 8],
%% --                            [3, 4, 5, 6, 5],
%% --                            [8, 4, 3, 4, 1]
%% --                        ],
%% --                        [ -- 5
%% --                            [2, 3, 3, 2, 3],
%% --                            [7, 8, 8, 7, 8],
%% --                            [5, 5, 5, 5, 5],
%% --                            [5, 3, 1, 5, 1]
%% --                        ],
%% --                        [ -- 6
%% --                            [4, 3, 2, 7, 8],
%% --                            [2, 8, 6, 5, 5],
%% --                            [6, 5, 4, 2, 3],
%% --                            [2, 3, 4, 5, 1]
%% --                        ],
%% --                        [ -- 7
%% --                            [5, 6, 5, 5, 5],
%% --                            [8, 8, 8, 7, 8],
%% --                            [3, 4, 3, 2, 3],
%% --                            [1, 4, 5, 5, 1]
%% --                        ],
%% --                        [ -- 8
%% --                            [3, 4, 4, 3, 4],
%% --                            [8, 8, 8, 8, 8], 
%% --                            [5, 6, 6, 5, 6],
%% --                            [8, 6, 4, 1, 4]
%% --                        ],
%% --                        [ -- 9
%% --                            [5, 6, 5, 4, 3],
%% --                            [8, 8, 8, 8, 8],
%% --                            [3, 4, 5, 6, 5],
%% --                            [1, 4, 3, 4, 1]
%% --                        ]
%% --                    ],
%% --                    --}}}4
%% --                    --{{{4 col 4, syl 6
%% --                    [ 
%% --                        [ -- 0
%% --                            [5, 5, 5,  3, 5, 5], 
%% --                            [8, 8, 7,  8, 7, 8],
%% --                            [3, 3, 2,  2, 2, 3], 
%% --                            [8, 8, 5,  6, 5, 1]
%% --                        ],
%% --                        [ -- 1
%% --                            [4, 4, 3,  2, 2, 2],
%% --                            [2, 2, 8,  7, 6, 7],
%% --                            [6, 6, 5,  5, 4, 5],
%% --                            [2, 2, 3,  5, 2, 5]
%% --                        ],
%% --                        [ -- 2
%% --                            [3, 2, 3,  5, 4, 5],
%% --                            [8, 7, 8,  2, 8, 2],
%% --                            [5, 5, 5,  2, 6, 7],
%% --                            [8, 5, 8,  7, 6, 5]
%% --                        ],
%% --                        [ -- 3
%% --                            [8, 8, 2,  3, 2, 3],
%% --                            [6, 5, 6,  8, 7, 8],
%% --                            [4, 5, 4,  5, 5, 5],
%% --                            [4, 3, 2,  1, 5, 1]
%% --                        ],
%% --                        [ -- 4
%% --                            [6, 5, 6,  5, 4, 3],
%% --                            [8, 8, 8,  8, 8, 8],
%% --                            [4, 3, 4,  5, 6, 5],
%% --                            [4, 8, 4,  3, 4, 1]
%% --                        ],
%% --                        [ -- 5
%% --                            [3, 8, 7,  8, 7, 8],
%% --                            [5, 5, 5,  3, 5, 5],
%% --                            [3, 3, 2,  8, 2, 3],
%% --                            [8, 8, 5,  6, 5, 1]
%% --                        ],
%% --                        [ -- 6
%% --                            [4, 4, 3,  2, 2, 2],
%% --                            [2, 2, 8,  7, 6, 7],
%% --                            [6, 6, 5,  5, 4, 5],
%% --                            [2, 2, 3,  5, 2, 5]
%% --                        ],
%% --                        [ -- 7
%% --                            [3, 2, 3,  5, 4, 5],
%% --                            [8, 7, 8,  2, 8, 2],
%% --                            [5, 5, 5,  2, 6, 7],
%% --                            [8, 5, 8,  7, 6, 5]
%% --                        ],
%% --                        [ -- 8
%% --                            [6, 4, 3,  2, 2, 3],
%% --                            [8, 2, 8,  8, 7, 8],
%% --                            [4, 6, 5,  6, 5, 5],
%% --                            [4, 2, 3,  4, 5, 1]
%% --                        ],
%% --                        [ -- 9
%% --                            [6, 5, 6,  5, 4, 3],
%% --                            [8, 8, 8,  8, 8, 8],
%% --                            [4, 3, 4,  5, 6, 5],
%% --                            [4, 8, 4,  3, 4, 1]
%% --                        ]
%% --                    ]
%% --                    --}}}4
%% --                
%% --                --{{{3 rperms
%% --                    --{{{4 col 2, syl 4
%% --                        [ 
%% --                        [ -- duple
%% --                            [Sb, Sb, Sb, Sb],
%% --                            [SbD, Mn, Sb, Sb],
%% --                            [Mn, Mn, Sb, Sb],
%% --                            [Sm, Sm, Sb, Mn],
%% --                            [MnR, Sb, Mn, Sb, Sb],
%% --                            [SmR, Mn, Sm, Mn, Mn],
%% --                            [Mn, Mn, Mn, Mn]
%% --                        ],
%% --                        [ -- triple major
%% --                            [Br, Sb, Br, Sb],
%% --                            [Br, Sb, BrD, BrD]
%% --                        ],
%% --                        [ -- triple minor>
%% --                            [Sb, Mn, Sb, Mn],
%% --                            [Sb, Mn, SbD, SbD]
%% --                        ]
%% --                    ],
%% --                    --{{{4 col 3, syl 5
%% --                    [ 
%% --                        [ -- duple
%% --                            [Sb, Mn, Mn, Sb, Sb],
%% --                            [Mn, Sb, Mn, Sb, Sb],
%% --                            [Sm, Fs, Fs, Sm, Sm],
%% --                            [Sm, Mn, Sm, Sb, Sb],
%% --                            [Mn, Sm, Sm, Mn, Mn],
%% --                            [SmR, Sm, Sm, Sm, Mn, Mn],
%% --                            [Sb, MnD, Sm, Mn, Mn],
%% --                            [MnR, Sb, Mn, Mn, Mn, Sb]
%% --                        ],
%% --                        [ -- triple major
%% --                            [Sb, Sb, Sb, Br, Sb],
%% --                            [BrR, Sb, Br, Sb, Br, Sb]
%% --                        ],
%% --                        [ -- triple minor>
%% --                            [Mn, Mn, Mn, Sb, Mn],
%% --                            [SbR, Mn, Sb, Mn, SbD, SbD]
%% --                        ]
%% --                    ],
%% --                    --}}}4
%% --                    --{{{4 col 4, syl 6
%% --                    [ 
%% --                        [ -- duple
%% --                            [Mn, Mn, Mn, Mn, Sb, Sb],
%% --                            [Sm, Sm, Sm, Sm, Mn, Mn],
%% --                            [SbD, Mn, Mn, Mn, Sb, Sb],
%% --                            [SmR, Mn, Sm, Mn, Mn, Mn, Mn],
%% --                            [MnR, Sb, Mn, Mn, Mn, Sb, Sb],
%% --                            [Sm, Sm, SmD, Fs, Mn, Mn],
%% --                            [MnD, Sm, Sm, Sm, Mn, Sb]
%% --                        ],
%% --                        [ -- triple major
%% --                            [Br, Sb, Br, Sb, BrD, BrD],
%% --                            [SbR, Sb, Sb, Br, Sb, Br, Sb],
%% --                            [Sb, Sb, Sb, Sb, Br, BrD]
%% --                        ],
%% --                        [ -- triple minor>
%% --                            [Sb, Mn, Sb, Mn, SbD, SbD],
%% --                            [MnR, Mn, Mn, Sb, Mn, SbD, SbD]
%% --                        ]
%% --                    ]
%% --                    --}}}4
%% --                ]
%% --                --}}}3
%% --            ) 
%% --            --}}}2
%% --        ]
%% ----            --{{{2 pinax 2 
%% ----            -- Voces polysyllabae, quae penultimam Breuem habet
%% ----            [ 
%% ----                --{{{3 vperms
%% ----                [
%% ----                    --{{{4 col 0, syl 2
%% ----                    [
%% ----                        [ -- 0
%% ----                            [2, 2],
%% ----                            [8, 7],
%% ----                            [5, 5],
%% ----                            [8, 5]
%% ----                        ],
%% ----                        [ -- 1
%% ----                            [2, 3],
%% ----                            [7, 8],
%% ----                            [5, 5],
%% ----                            [5, 1]
%% ----                        ],
%% ----                        [ -- 2
%% ----                            [3, 3],
%% ----                            [8, 8],
%% ----                            [5, 5],
%% ----                            [8, 1]
%% ----                        ],
%% ----                        [ -- 3
%% ----                            [6, 5],
%% ----                            [1, 1],
%% ----                            [4, 3],
%% ----                            [4, 8]
%% ----                        ],
%% ----                        [ -- 4
%% ----                            [2, 3],
%% ----                            [5, 5],
%% ----                            [7, 8],
%% ----                            [5, 1]
%% ----                        ],
%% ----                        [ -- 5
%% ----                            [3, 2],
%% ----                            [8, 7],
%% ----                            [5, 5],
%% ----                            [8, 5]
%% ----                        ],
%% ----                        [ -- 6
%% ----                            [5, 5],
%% ----                            [8, 7],
%% ----                            [3, 2],
%% ----                            [8, 5]
%% ----                        ],
%% ----                        [ -- 7
%% ----                            [2, 3],
%% ----                            [7, 8],
%% ----                            [5, 5],
%% ----                            [5, 1]
%% ----                        ],
%% ----                        [ -- 8
%% ----                            [6, 5],
%% ----                            [8, 8],
%% ----                            [4, 3],
%% ----                            [4, 8]
%% ----                        ]
%% ----                    ],
%% ----                    --}}}4
%% ----                    --{{{4 col 1, syl 3
%% ----                    [
%% ----                        [ -- 0
%% ----                            [5, 5, 5],
%% ----                            [7, 7, 8],
%% ----                            [2, 2, 3],
%% ----                            [5, 5, 1]
%% ----                        ],
%% ----                        [ -- 1
%% ----                            [4, 4, 5],
%% ----                            [6, 6, 7],
%% ----                            [2, 2, 3],
%% ----                            [2, 2, 5]
%% ----                        ],
%% ----                        [ -- 2
%% ----                            [3, 3, 4],
%% ----                            [8, 8, 8],
%% ----                            [5, 5, 6],
%% ----                            [1, 1, 4]
%% ----                        ],
%% ----                        [ -- 3
%% ----                            [2, 2, 3],
%% ----                            [7, 7, 8],
%% ----                            [5, 5, 5],
%% ----                            [5, 5, 1]
%% ----                        ],
%% ----                        [ -- 4
%% ----                            [4, 4, 3],
%% ----                            [8, 8, 8],
%% ----                            [6, 6, 5],
%% ----                            [4, 4, 8]
%% ----                        ], 
%% ----                        [ -- 5
%% ----                            [5, 5, 5],
%% ----                            [7, 7, 8],
%% ----                            [2, 2, 3],
%% ----                            [5, 5, 1]
%% ----                        ],
%% ----                        [ -- 6
%% ----                            [3, 3, 4],
%% ----                            [8, 8, 8],
%% ----                            [5, 5, 6],
%% ----                            [8, 8, 4]
%% ----                        ],
%% ----                        [ -- 7 
%% ----                            [4, 4, 2],
%% ----                            [6, 6, 7],
%% ----                            [2, 2, 2],
%% ----                            [2, 2, 5]
%% ----                        ],
%% ----                        [ -- 8
%% ----                            [2, 2, 3],
%% ----                            [7, 7, 8],
%% ----                            [5, 5, 5],
%% ----                            [5, 5, 1]
%% ----                        ],
%% ----                        [ -- 9
%% ----                            [6, 6, 5],
%% ----                            [8, 8, 8],
%% ----                            [4, 4, 3],
%% ----                            [4, 4, 1]
%% ----                        ]
%% ----                    ]
%% ----                    --}}}4
%% ------ TODO start here
%% ----                ],
%% ----                --}}}3
%% ----                --{{{3 rperms
%% ----                [
%% ----                ]
%% ----                --}}}3
%% ----            ]
%% ----            --}}}2
%% ----        ]
%% ----        --}}}1
%%}}}1
