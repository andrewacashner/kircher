% vim: set foldmethod=marker :

% TODO
% - account for rests
% - account for meters

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
import Data.List
import Data.Maybe
\end{code}

\section{Enumeration Types}
\begin{code}
data Accid = Fl | Na | Sh 
    deriving (Enum, Show)

data Dur = Br | Sb | Mn | Sm | Fs
    | BrD | SbD | MnD | SmD | FsD -- dotted
    | BrR | SbR | MnR | SmR | FsR -- rests
    deriving (Enum, Show)

data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Show)
\end{code}
% }}}2

% {{{2 pitch, vperm, rperm
\section{The @Pitch@ Data Type}
@Pitch@ contains all the information for a single musical note, equivalent to
what is encoded in an MEI @<note>@ element (with the same attributes).

@Voice@ is a list of @Pitch@es (i.e., a single line of musical notes), and
@Chorus@ is a list of @Voices@ (simultaneous counterpoint).

\begin{code}
data Pitch = Pitch {
        pnum  :: Int,
        oct   :: Int,
        accid :: Accid,
        dur   :: Dur
    } deriving (Show)

type Voice  = [Pitch]
type Chorus = [Voice]
type Music  = [Chorus]
\end{code}

\subsection{Standardize Pitch}
Adjust a pitch to normal bounds.
If a pitch is given the value |{pnum = 7, oct = 4}|, it should be converted to
|{pnum = 0, oct = 5}|.

\begin{code}
stdPitch :: Pitch -> Pitch
stdPitch p =
    let pnum1 = pnum p in 
        if pnum1 >= 0 && pnum1 <= 6
            then id p
            else 
                let 
                    pnumAbsolute = (oct p) * 7 + pnum1
                    pnumAdjust = quotRem pnumAbsolute 7
                    pnum2 = (snd pnumAdjust)
                    oct2 = (fst pnumAdjust)
                in Pitch pnum2 oct2 (accid p) (dur p)

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

vpermIndex :: Vperm -> Int -> VoiceName -> Int -> Int
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
getOctave :: VoiceName -> Int -> Int
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
position in the note series.
Standardize the pitch in case the pnum is out of range (i.e., adjust octave as
needed).

\begin{code}
perm2pitch :: Vperm -> Int -> VoiceName 
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

\subsubsection{Building Up a Chorus}
Make pitches out of a whole vperm/rperm combination and put them together
as a chorus of voices.

Turn all the knums in a row into a voice made of pitches.
\begin{code}

perm2Chorus :: Vperm -> Int -> Rperm -> Int -> Int -> Chorus
perm2Chorus vperm vIndex rperm rIndex mode =
    map (\ row -> permRow2voice row thisRperm 
            (toEnum (fromJust $ elemIndex row thisVperm))
            -- this is not a good way to get the voice number
            mode) 
        thisVperm
    where
        thisVperm = vperm !! vIndex
        thisRperm = rperm !! rIndex

permRow2voice :: [Int] -> [Dur] -> VoiceName -> Int -> Voice
permRow2voice vpermRow rpermRow voice mode =
    map (\ pair -> knumDur2Pitch pair voice mode) (zip vpermRow rpermRow)

knumDur2Pitch :: (Int, Dur) -> VoiceName -> Int -> Pitch
knumDur2Pitch pair voice mode = 
    stdPitch Pitch {
        pnum    = thispnum,
        oct     = getOctave voice thispnum,
        accid   = getAccid mode knum,
        dur     = (snd pair)
    } where
        knum    = (fst pair)
        thispnum = knum2pnum knum mode 

\end{code}
%}}}2

% {{{2 output
\section{Output to music-notation language}
Convert to Lilypond.%
\footnote{MEI would be desirable but typesetting capacity is wanting.}

\begin{code}
pitch2ly :: Pitch -> String
pitch2ly (Pitch pnum oct accid dur) = lyPname : lyAccid ++ lyOct ++ lyDur
    where
        lyPname = "cdefab" !! pnum
        lyOct   = lyOctave oct
        lyAccid = ["es", "", "is"] !! fromEnum accid
        lyDur   = ["\\breve", "1", "2", "4", "8"] !! fromEnum dur
\end{code}

We have to convert Helmholtz octaves to tick marks.
\begin{code}
lyOctave :: Int -> String
lyOctave n 
    | n > 3  = replicate (n - 3) '\''
    | n < 3  = replicate (3 - n) ','
    | otherwise = ""
\end{code}

\subsection{Printing the music lists}

We can use the same function @lyStr@ to print each hierarchical layer of music
list within the necessary Lilypond structures, as follows:

\begin{tabular}{ll}
@Pitch@  & note \\
@Voice@  & @Voice@ within @Staff@ \\
@Chorus@ & @ChoirStaff@ \\
@Music@  & @score@
\end{tabular}

Like everything with Lilypond, setting the indent levels is inconsistent and
frustrating, but @lyIndent@ takes care of it.%
\footnote{TBD there is an extra space after each Voice group, probably because
of @unwords@.}

\begin{code}
lyStr :: (x -> String) -> [x] -> String -> String -> String
lyStr fn ls start end = start ++ unwords (map fn ls) ++ end

lyIndent :: Int -> String
lyIndent n = replicate (n * 2) ' '

voice2ly :: Voice -> String
voice2ly v = lyStr pitch2ly v start end
    where
        start = lyIndent 3 ++ "\\new Staff\n" ++
                lyIndent 3 ++ "<<\n" ++
                lyIndent 4 ++ "\\new Voice { "
        end   = " }\n" ++
                lyIndent 3 ++ ">>\n"

chorus2ly :: Chorus -> String
chorus2ly ch = lyStr voice2ly ch start end
    where
        start = lyIndent 2 ++ "\\new ChoirStaff\n" ++
                lyIndent 2 ++ "<<\n"
        end   = lyIndent 2 ++ ">>\n"


lyScore :: Music -> String
lyScore music = lyStr chorus2ly music start end
    where
        start = "\\score {\n  <<\n"  
        end   = "  >>\n}\n"


\end{code}

% }}}2
%}}}1

\end{document}
