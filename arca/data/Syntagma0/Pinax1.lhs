\documentclass{haskell}
%include polycode.fmt
\title{Arca Syntagma 0 Pinax 1}
\author{Kircher}
\date{1650}

\begin{document}
%{{{1 module
\section{Module}
\begin{code}
module Syntagma0.Pinax1 (s0p1) where
import Data.Vector (fromList)
import Kircher 
\end{code}
%}}}1

\section{Pinax 1}
``Voces polysyllabae, quae penultimam Breuem habet''
%{{{1 pinax 1
\begin{code}
s0p1 = fromList [c0, c1, c2, c3, c4] :: Pinax
\end{code}

\subsection{Column 0 (2 syllables)}
%{{{2 col0
\begin{code}
c0 = (c0v, c0r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c0v :: VpermTable
c0v = fromList2D
    [
        [ -- 0
            [2, 2],
            [8, 7],
            [5, 5],
            [8, 5]
        ],
        [ -- 1
            [2, 3],
            [7, 8],
            [5, 5],
            [5, 1]
        ],
        [ -- 2
            [3, 3],
            [8, 8],
            [5, 5],
            [8, 1]
        ],
        [ -- 3
            [6, 5],
            [1, 1],
            [4, 3],
            [4, 8]
        ],
        [ -- 4
            [2, 3],
            [5, 5],
            [7, 8],
            [5, 1]
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
            [2, 3],
            [7, 8],
            [5, 5],
            [5, 1]
        ],
        [ -- 8
            [6, 5],
            [8, 8],
            [4, 3],
            [4, 8]
        ],
        [ -- 9
            [0, 0],
            [0, 0],
            [0, 0],
            [0, 0] -- XXX missing, check source
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
            [MnD, Sm],
            [SmD, Fs],
            [Sm, Sm]
        ],
        [ -- triple major
            [Br, Sb],
            [BrR, Sb, BrD]
        ],
        [ -- triple minor
            [Sb, Mn],
            [MnR, Mn, Mn]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsection{Column 1 (3 syllables)}
%{{{2 col1
\begin{code}
c1 = (c1v, c1r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c1v :: VpermTable
c1v = fromList2D 
    [
        [ -- 0
            [5, 5, 5],
            [7, 7, 8],
            [2, 2, 3],
            [5, 5, 1]
        ],
        [ -- 1
            [4, 4, 5],
            [6, 6, 7],
            [2, 2, 3],
            [2, 2, 5]
        ],
        [ -- 2
            [3, 3, 4],
            [8, 8, 8],
            [5, 5, 6],
            [1, 1, 4]
        ],
        [ -- 3
            [2, 2, 3],
            [7, 7, 8],
            [5, 5, 5],
            [5, 5, 1]
        ],
        [ -- 4
            [4, 4, 3],
            [8, 8, 8],
            [6, 6, 5],
            [4, 4, 8]
        ], 
        [ -- 5
            [5, 5, 5],
            [7, 7, 8],
            [2, 2, 3],
            [5, 5, 1]
        ],
        [ -- 6
            [3, 3, 4],
            [8, 8, 8],
            [5, 5, 6],
            [8, 8, 4]
        ],
        [ -- 7 
            [4, 4, 2],
            [6, 6, 7],
            [2, 2, 2],
            [2, 2, 5]
        ],
        [ -- 8
            [2, 2, 3],
            [7, 7, 8],
            [5, 5, 5],
            [5, 5, 1]
        ],
        [ -- 9
            [6, 6, 5],
            [8, 8, 8],
            [4, 4, 3],
            [4, 4, 1]
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
            [SbD, Mn, Sb],
            [SmD, Mn, Sb],
            [SmD, Fs, Mn],
            [SmR, Mn, Sm, Sb],
            [MnR, Sb, Mn, Sb],
            [SmR, SmD, Fs, Sm]
        ],
        [ -- triple major
            [SbD, Mn, Sb],
            [Sb, Sb, Sb],
            [Br, Sb, BrD]
        ],
        [ -- triple minor
            [MnD, Sm, Mn],
            [Mn, Mn, Mn],
            [Sb, Mn, SbD]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsection{Column 2 (4 syllables)}
%{{{2 col2
\begin{code}
c2 = (c2v, c2r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c2v :: VpermTable
c2v = fromList2D
    [
        [ -- 0
            [3, 2, 8, 7],
            [8, 7, 5, 5],
            [5, 5, 3, 2],
            [8, 5, 8, 5]
        ],
        [ -- 1
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [8, 4, 5, 1]
        ],
        [ -- 2
            [2, 3, 2, 2],
            [7, 8, 6, 7],
            [5, 5, 4, 5],
            [5, 8, 2, 5]
        ],
        [ -- 3
            [3, 4, 4, 3],
            [8, 8, 8, 8],
            [5, 6, 6, 5],
            [1, 4, 4, 1]
        ],
        [ -- 4
            [4, 3, 5, 5],
            [6, 6, 7, 8],
            [2, 8, 2, 3],
            [2, 6, 5, 1]
        ],
        [ -- 5
            [3, 4, 4, 3],
            [8, 8, 8, 8],
            [5, 6, 6, 5],
            [8, 4, 4, 1]
        ],
        [ -- 6: cf perm 4
            [4, 3, 5, 5],
            [6, 6, 7, 8],
            [2, 8, 2, 3],
            [2, 6, 5, 1]
        ],
        [ -- 7
            [4, 3, 5, 5],
            [6, 6, 7, 8],
            [2, 8, 2, 3],
            [2, 6, 5, 1]
        ],
        [ -- 8
            [3, 2, 7, 8],
            [8, 6, 5, 5],
            [5, 4, 2, 3],
            [1, 4, 5, 1]
        ],
        [ -- 9
            [3, 4, 4, 3],
            [8, 8, 8, 8],
            [5, 6, 6, 5],
            [1, 4, 4, 1]
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
            [Sb, SbD, Mn, Sb],
            [Sb, MnD, Sm, Sb],
            [Mn, MnD, Sm, Mn],
            [Mn, SmD, Fs, Sb],
            [Mn, Sb, Mn, Sb],
            [SmR, Sm, SmD, Fs, Sb]
        ],
        [ -- triple major
            [Sb, Sb, Sb, BrD],
            [BrR, Sb, SbD, Mn, Sb],
            [BrR, Sm, Br, Sm, BrD]
        ],
        [ -- triple minor
            [Mn, Mn, Mn, SbD],
            [SbR, Mn, MnD, Sm, SbD],
            [SbR, Mn, Sb, Mn, SbD]
        ]
    ]
\end{code}
%}}}3
%}}}2

\subsection{Column 3 (5 syllables)}
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
        [7, 7, 8, 2, 3],
        [5, 5, 5, 7, 8],
        [2, 2, 3, 4, 5],
        [5, 5, 3, 2, 1]
      ],
      [ -- 1
        [3, 3, 4, 3, 4],
        [8, 8, 8, 7, 8],
        [5, 5, 6, 7, 6],
        [8, 8, 6, 5, 4]
      ],
      [ -- 2
        [4, 4, 3, 2, 3],
        [8, 2, 8, 7, 8],
        [6, 6, 5, 5, 5],
        [4, 2, 3, 5, 1]
      ],
      [ -- 3
        [7, 8, 2, 2, 2],
        [5, 5, 6, 6, 7],
        [2, 3, 4, 4, 5],
        [5, 3, 2, 2, 5]
      ],
      [ -- 4
        [8, 2, 7, 7, 8],
        [6, 6, 5, 5, 5],
        [3, 4, 2, 2, 3],
        [6, 4, 5, 5, 1]
      ],
      [ -- 5
        [7, 8, 7, 7, 8],
        [5, 5, 5, 5, 5],
        [2, 3, 2, 2, 3],
        [5, 1, 5, 5, 1]
      ],
      [ -- 6
        [3, 2, 7, 7, 8],
        [7, 6, 5, 5, 5],
        [5, 4, 2, 2, 3],
        [3, 4, 5, 5, 1]
      ],
      [ -- 7
        [5, 6, 5, 5, 5],
        [8, 8, 8, 8, 8],
        [3, 4, 2, 2, 3],
        [1, 4, 5, 5, 1]
      ],
      [ -- 8
        [8, 2, 7, 7, 8],
        [6, 6, 5, 5, 5],
        [3, 4, 2, 2, 3],
        [6, 4, 5, 5, 1]
      ],
      [ -- 9
        [6, 5, 4, 4, 3],
        [8, 8, 8, 8, 8],
        [4, 5, 6, 6, 8],
        [4, 1, 4, 4, 1]
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
        [SbD, Mn, SbD, Mn, Sb],
        [Mn, Mn, MnD, Sm, Sb],
        [Sm, Sm, SmD, Fs, Sb],
        [MnR, Sb, Mn, SbD, Mn, Sb],
        [SmR, Mn, Sm, SmD, Fs, Mn],
        [MnD, Sm, MnD, Sm, Sb]
      ],
      [ -- triple major
        [Br, Sb, Br, Sb, BrD],
        [Br, Sb, SbD, Mn, Sb],
        [SbR, Sb, Sb, SbD, Mn, Sb]
      ],
      [ -- triple minor
        [Sb, Mn, Sb, Mn, SbD],
        [Sb, Mn, MnD, Sm, Mn],
        [MnR, Mn, Mn, MnD, Sm, Mn]
      ]
    ]
\end{code}
%}}}3
%}}}2

\subsection{Column 4 (6 syllables)}
%{{{2 col4
\begin{code}
c4 = (c4v, c4r) :: Column

\end{code}

%{{{3 vperms
\begin{code}
c4v :: VpermTable
c4v = fromList2D
    [
      [ -- 0
        [2, 3, 3,  2, 2, 3],
        [7, 8, 8,  7, 7, 8],
        [5, 5, 5,  5, 5, 5],
        [5, 3, 1,  5, 5, 1]
      ],
      [ -- 1
        [5, 6, 5,  5, 5, 5],
        [8, 8, 8,  7, 7, 8],
        [3, 4, 3,  2, 2, 3],
        [8, 4, 8,  5, 5, 1]
      ],
      [ -- 2
        [2, 3, 2,  2, 2, 2],
        [7, 8, 7,  6, 6, 7],
        [5, 5, 5,  4, 4, 5],
        [5, 1, 5,  2, 2, 5]
      ],
      [ -- 3
        [5, 4, 3,  2, 2, 3],
        [3, 2, 1,  1, 7, 1],
        [8, 6, 5,  6, 5, 5],
        [1, 2, 3,  4, 5, 1]
      ],
      [ -- 4 
        [6, 6, 5,  6, 6, 5],
        [8, 8, 8,  8, 8, 8],
        [4, 4, 3,  4, 4, 3],
        [4, 4, 8,  4, 4, 8]
      ],
      [ -- 5 
        [2, 3, 3,  2, 2, 3],
        [7, 8, 8,  7, 7, 8],
        [5, 5, 5,  5, 5, 5],
        [5, 3, 1,  5, 5, 1]
      ],
      [ -- 6 
        [5, 4, 3,  2, 2, 3],
        [8, 8, 8,  7, 7, 8],
        [3, 6, 5,  5, 5, 5],
        [1, 4, 1,  5, 5, 1]
      ],
      [ -- 7 
        [2, 2, 2,  2, 2, 2],
        [7, 6, 7,  6, 6, 7],
        [5, 4, 5,  4, 4, 5],
        [5, 2, 5,  2, 2, 5]
      ],
      [ -- 8 
        [8, 4, 5,  6, 6, 5],
        [6, 8, 8,  8, 8, 8],
        [4, 6, 3,  4, 4, 3],
        [4, 4, 1,  4, 4, 1]
      ],
      [ -- 9
        [5, 4, 3,  2, 2, 3],
        [7, 6, 8,  8, 7, 8],
        [3, 2, 5,  6, 5, 5],
        [5, 2, 3,  4, 5, 1]
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
        [Sb, Mn, Mn, MnD, Sm, Sb],
        [Sb, Mn, Mn, SbD, Mn, Sb],
        [Mn, Sm, Sm, SmD, Fs, Mn],
        [SmR, Sm, Sm, Sm, SmD, Fs, Mn],
        [MnR, Mn, Mn, Mn, MnD, Sm, Sb],
        [Sm, Fs, Fs, SmD, Fs, Sb],
        [Mn, Sb, Mn, MnD, Sm, Sb]
      ],
      [ -- triple major
        [Sb, Sb, Sb, SbD, Mn, Sb],
        [Sb, Sb, Sb, Br, Sb, BrD],
        [BrR, Sb, Br, Sb, Br, Sb, BrD]
      ],
      [ -- triple minor
        [Mn, Mn, Mn, MnD, Sm, Mn],
        [Mn, Mn, Mn, Sb, Mn, SbD]
      ]
    ]
\end{code}
%}}}3
%}}}2
%}}}1
\end{document}
