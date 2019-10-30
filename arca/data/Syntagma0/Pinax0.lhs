\documentclass{haskell}
%include polycode.fmt

\title{Arca Syntagma 0}
\author{Kircher}
\date{1650}

\begin{document}
%{{{1 module
\section{Module}
\begin{code}
module Syntagma0.Pinax0 (s0p0) where
import Data.Vector (fromList)
import Kircher 
\end{code}
%}}}1

\subsection{Pinax 0}
``Pinax I. Voces Polysyllabae quae penultimam Longam habent.''
%{{{1 pinax 0
\begin{code}
s0p0 = fromList [c0, c1, c2, c3, c4] :: Pinax
\end{code}

\subsubsection{Col 0 (2 syllables)}
%{{{2 col 0
\begin{code}
c0 = (c0v, c0r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c0v :: VpermTable
c0v = fromList2D
    [
        [ -- 0
            [5, 5],
            [7, 8],
            [2, 3],
            [5, 1]
        ],
        [ -- 1
            [5, 5],
            [7, 7],
            [2, 2],
            [5, 5]
        ],
        [ -- 2
            [5, 5],
            [8, 8],
            [3, 3],
            [8, 8]
        ],
        [ -- 3
            [4, 4],
            [6, 6],
            [8, 8],
            [4, 4]
        ],
        [ -- 4
            [4, 3],
            [8, 8],
            [6, 5],
            [4, 8]
        ],
        [ -- 5
            [3, 2],
            [8, 7],
            [5, 5],
            [8, 5]
        ],
        [ -- 6
            [5, 5],
            [8, 7],
            [3, 2],
            [8, 5]
        ],
        [ -- 7
            [5, 5],
            [7, 8],
            [2, 3],
            [5, 1] 
        ],
        [ -- 8
            [2, 3],
            [7, 8],
            [5, 5],
            [5, 1]
        ],
        [ -- 9
            [6, 5],
            [8, 8],
            [4, 3],
            [4, 1]
        ]
    ] 
\end{code}
%}}}3

%{{{3 rperms
\begin{code}
c0r :: RpermTable
c0r = fromList2D
    [
        [ -- duple
            [Sb, Sb],
            [Mn, Mn],
            [Sm, Sm],
            [Fs, Fs],
            [SbD, Mn],
            [MnD, Sm],
            [SmD, Fs]
        ],
        [ -- triple major
            [Br, Sb],
            [BrD, BrD]
        ],
        [ -- triple minor
            [Sb, Mn]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsubsection{Col 1 (3 syllables)}
%{{{2 col 1
\begin{code}
c1 = (c1v, c1r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c1v :: VpermTable
c1v = fromList2D
    [ 
         [ -- 0
            [3, 2, 3],
            [8, 7, 8],
            [5, 5, 5],
            [1, 5, 1]
        ],
        [ -- 1
            [4, 2, 8],
            [8, 7, 6],
            [6, 5, 3],
            [4, 5, 6]
        ],
        [ -- 2
            [4, 3, 4],
            [8, 8, 8],
            [6, 5, 5],
            [4, 1, 4]
        ],
        [ -- 3
            [2, 2, 2],
            [7, 6, 7],
            [5, 4, 5],
            [5, 2, 5]
        ],
        [ -- 4
            [6, 5, 5],
            [8, 7, 7],
            [4, 2, 2],
            [4, 5, 1]
        ],
        [ -- 5
            [5, 4, 3],
            [8, 8, 8],
            [3, 6, 5],
            [8, 4, 8]
        ],
        [ -- 6
            [5, 5, 5],
            [8, 7, 8],
            [3, 2, 3],
            [1, 5, 1]
        ],
        [ -- 7
            [6, 5, 6],
            [8, 8, 8],
            [4, 3, 4],
            [4, 8, 4]
        ],
        [ -- 8
            [3, 2, 3],
            [8, 7, 8],
            [5, 5, 5],
            [1, 5, 1]
        ],
        [ -- 9
            [6, 5, 5],
            [8, 7, 8],
            [4, 2, 3],
            [4, 5, 1]
        ]
    ]
\end{code}
%}}}3

%{{{3 rperms
\begin{code}
c1r :: RpermTable
c1r = fromList2D
    [
        [ -- duple
            [Sb, Sb, Sb],
            [Mn, Sb, Mn],
            [SbR, SmR, Sm, Sb, Sb],
            [MnR, Mn, SbD, Mn],
            [Sb, Mn, Mn],
            [SmR, Sm, Sm, Sm],
            [Sm, Mn, Sm]
        ],
        [ -- triple major
            [Sb, Sb, Sb],
            [SbR, Sb, Br, Sb]
        ],
        [ -- triple minor
            [Mn, Mn, Mn],
            [SbR, Mn, Sb, Mn]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsubsection{Col 2 (4 syllables)}
%{{{2 col 2
\begin{code}
c2 = (c2v, c2r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c2v :: VpermTable
c2v = fromList2D
    [ 
        [ -- 0
            [5, 5, 5, 5],
            [7, 8, 7, 8],
            [2, 3, 2, 3],
            [5, 8, 5, 1]
        ],
        [ -- 1
            [3, 3, 2, 3],
            [8, 8, 7, 8],
            [5, 5, 5, 5],
            [8, 8, 5, 1]
        ],
        [ -- 2
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [3, 4, 5, 1]
        ],
        [ -- 3
            [5, 6, 5, 5],
            [8, 8, 7, 8],
            [3, 4, 2, 3],
            [8, 4, 5, 1]
        ],
        [ -- 4
            [6, 5, 6, 5],
            [8, 8, 8, 8],
            [4, 3, 4, 3],
            [4, 8, 4, 8]
        ],
        [ -- 5
            [2, 3, 2, 3],
            [7, 8, 7, 8],
            [5, 5, 5, 5],
            [5, 1, 5, 1]
        ],
        [ -- 6
            [3, 3, 2, 3],
            [8, 8, 7, 8],
            [5, 5, 5, 5],
            [8, 8, 5, 8]
        ],
        [ -- 7
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [3, 4, 5, 1]
        ],
        [ -- 8
            [2, 3, 2, 3],
            [7, 8, 7, 8],
            [5, 5, 5, 5],
            [5, 8, 5, 1]
        ],
        [ -- 9
            [4, 5, 6, 5],
            [8, 8, 8, 8],
            [6, 5, 4, 3],
            [4, 3, 4, 1]
        ]
    ]

\end{code}
%}}}3 

%{{{3 rperms
\begin{code}
c2r :: RpermTable
c2r = fromList2D
    [ 
        [ -- duple
            [Sb, Sb, Sb],
            [Mn, Sb, Mn],
            [SbR, SmR, Sm, Sb, Sb],
            [MnR, Mn, SbD, Mn],
            [Sb, Mn, Mn],
            [SmR, Sm, Sm, Sm],
            [Sm, Mn, Sm]
        ],
        [ -- triple major
            [Sb, Sb, Sb],
            [SbR, Sb, Br, Sb]
        ],
        [ -- triple minor
            [Mn, Mn, Mn],
            [SbR, Mn, Sb, Mn]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsubsection{Col 3 (5 syllables)}
%{{{2 col3
\begin{code}
c3 = (c3v, c3r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c3v :: VpermTable
c3v = fromList2D
    [ 
        [ -- 0
            [2, 3, 3, 2, 3],
            [7, 8, 8, 7, 8],
            [5, 5, 5, 5, 5],
            [5, 3, 1, 5, 1]
        ],
        [ -- 1
             -- XXX cn: last s note in kircher is 5
            [5, 6, 6, 5, 6], 
            [7, 8, 8, 8, 8], 
            [3, 3, 4, 3, 4],
            [3, 6, 4, 8, 4]
        ],
        [ -- 2
            [3, 4, 3, 2, 3],
             -- XXX cn: penultimate a note in kircher is 8
            [8, 8, 8, 7, 8], 
            [5, 6, 5, 5, 5],
            [8, 4, 8, 5, 1]
        ],
        [ -- 3
            [2, 8, 2, 7, 8],
            [6, 5, 6, 5, 5],
            [4, 3, 2, 2, 3],
            [2, 3, 4, 5, 1]
        ],
        [ -- 4
            [5, 6, 5, 4, 3],
            [8, 8, 8, 8, 8],
            [3, 4, 5, 6, 5],
            [8, 4, 3, 4, 1]
        ],
        [ -- 5
            [2, 3, 3, 2, 3],
            [7, 8, 8, 7, 8],
            [5, 5, 5, 5, 5],
            [5, 3, 1, 5, 1]
        ],
        [ -- 6
            [4, 3, 2, 7, 8],
            [2, 8, 6, 5, 5],
            [6, 5, 4, 2, 3],
            [2, 3, 4, 5, 1]
        ],
        [ -- 7
            [5, 6, 5, 5, 5],
            [8, 8, 8, 7, 8],
            [3, 4, 3, 2, 3],
            [1, 4, 5, 5, 1]
        ],
        [ -- 8
            [3, 4, 4, 3, 4],
            [8, 8, 8, 8, 8], 
            [5, 6, 6, 5, 6],
            [8, 6, 4, 1, 4]
        ],
        [ -- 9
            [5, 6, 5, 4, 3],
            [8, 8, 8, 8, 8],
            [3, 4, 5, 6, 5],
            [1, 4, 3, 4, 1]
        ]
    ]
\end{code}
%}}}3

%{{{3 rperms
\begin{code}
c3r :: RpermTable
c3r = fromList2D
    [ 
        [ -- duple
            [Sb, Mn, Mn, Sb, Sb],
            [Mn, Sb, Mn, Sb, Sb],
            [Sm, Fs, Fs, Sm, Sm],
            [Sm, Mn, Sm, Sb, Sb],
            [Mn, Sm, Sm, Mn, Mn],
            [SmR, Sm, Sm, Sm, Mn, Mn],
            [Sb, MnD, Sm, Mn, Mn],
            [MnR, Sb, Mn, Mn, Mn, Sb]
        ],
        [ -- triple major
            [Sb, Sb, Sb, Br, Sb],
            [BrR, Sb, Br, Sb, Br, Sb]
        ],
        [ -- triple minor
            [Mn, Mn, Mn, Sb, Mn],
            [SbR, Mn, Sb, Mn, SbD, SbD]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsubsection{Col 4 (6 syllables)}
%{{{2 col 4
\begin{code}
c4 = (c4v, c4r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c4v :: VpermTable
c4v = fromList2D
    [ 
        [ -- 0
            [5, 5, 5,  3, 5, 5], 
            [8, 8, 7,  8, 7, 8],
            [3, 3, 2,  2, 2, 3], 
            [8, 8, 5,  6, 5, 1]
        ],
        [ -- 1
            [4, 4, 3,  2, 2, 2],
            [2, 2, 8,  7, 6, 7],
            [6, 6, 5,  5, 4, 5],
            [2, 2, 3,  5, 2, 5]
        ],
        [ -- 2
            [3, 2, 3,  5, 4, 5],
            [8, 7, 8,  2, 8, 2],
            [5, 5, 5,  2, 6, 7],
            [8, 5, 8,  7, 6, 5]
        ],
        [ -- 3
            [8, 8, 2,  3, 2, 3],
            [6, 5, 6,  8, 7, 8],
            [4, 5, 4,  5, 5, 5],
            [4, 3, 2,  1, 5, 1]
        ],
        [ -- 4
            [6, 5, 6,  5, 4, 3],
            [8, 8, 8,  8, 8, 8],
            [4, 3, 4,  5, 6, 5],
            [4, 8, 4,  3, 4, 1]
        ],
        [ -- 5
            [3, 8, 7,  8, 7, 8],
            [5, 5, 5,  3, 5, 5],
            [3, 3, 2,  8, 2, 3],
            [8, 8, 5,  6, 5, 1]
        ],
        [ -- 6
            [4, 4, 3,  2, 2, 2],
            [2, 2, 8,  7, 6, 7],
            [6, 6, 5,  5, 4, 5],
            [2, 2, 3,  5, 2, 5]
        ],
        [ -- 7
            [3, 2, 3,  5, 4, 5],
            [8, 7, 8,  2, 8, 2],
            [5, 5, 5,  2, 6, 7],
            [8, 5, 8,  7, 6, 5]
        ],
        [ -- 8
            [6, 4, 3,  2, 2, 3],
            [8, 2, 8,  8, 7, 8],
            [4, 6, 5,  6, 5, 5],
            [4, 2, 3,  4, 5, 1]
        ],
        [ -- 9
            [6, 5, 6,  5, 4, 3],
            [8, 8, 8,  8, 8, 8],
            [4, 3, 4,  5, 6, 5],
            [4, 8, 4,  3, 4, 1]
        ]
    ]
\end{code}
%}}}3

%{{{3 rperms
\begin{code}
c4r :: RpermTable
c4r = fromList2D
    [ 
        [ -- duple
            [Mn, Mn, Mn, Mn, Sb, Sb],
            [Sm, Sm, Sm, Sm, Mn, Mn],
            [SbD, Mn, Mn, Mn, Sb, Sb],
            [SmR, Mn, Sm, Mn, Mn, Mn, Mn],
            [MnR, Sb, Mn, Mn, Mn, Sb, Sb],
            [Sm, Sm, SmD, Fs, Mn, Mn],
            [MnD, Sm, Sm, Sm, Mn, Sb]
        ],
        [ -- triple major
            [Br, Sb, Br, Sb, BrD, BrD],
            [SbR, Sb, Sb, Br, Sb, Br, Sb],
            [Sb, Sb, Sb, Sb, Br, BrD]
        ],
        [ -- triple minor
            [Sb, Mn, Sb, Mn, SbD, SbD],
            [MnR, Mn, Mn, Sb, Mn, SbD, SbD]
        ]
    ]
\end{code}
%}}}3
%}}}2
%}}}1
\end{document}

