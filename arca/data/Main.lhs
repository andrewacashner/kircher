\documentclass{haskell}
%include polycode.fmt

\title{Arca musarithmica}
\author{Athanasii Kircheri Societatis Iesu}
\date{MDCL}

\begin{document}
\section{Module}
\begin{code}
module Main where
import Data.Vector (fromList)
import Kircher 
import Syntagma0.Pinax0 (s0p0)
import Syntagma0.Pinax1 (s0p1)
\end{code}

\section{Arca}
\begin{code}
arca = fromList [s0] :: Arca
\end{code}

\section{Syntagma 0}
``Syntagma I. Melothesias siue Contrapuncti simplicis.''
\begin{code}
s0 = fromList [s0p0, s0p1] :: Syntagma
\end{code}

\section{Main}
Test pulling data from ark.
\begin{code}
main :: IO()
main = do
    putStrLn(show (getMusic arca Simple Long 3 Duple Soprano 1))
    putStrLn(show (getMusic arca Simple Short 5 TripleMinor Tenor 1))

\end{code}

\end{document}
