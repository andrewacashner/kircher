{- 
 - The Data of Kircher's \emph{Arca musarithmica}}
 -}

module Modules.Arca where
import Data.Vector (Vector, (!), fromList)

-- * Data types

-- ** Equivalents of Kircher's Rods and Tables

-- *** Duration values
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

-- *** Elements of the ark
type Vperm      = [Int]
type VpermChoir = Vector (Vperm)
type VpermTable = Vector (VpermChoir)

type Rperm      = [Dur]
type RpermMeter = Vector (Rperm)
type RpermTable = Vector (RpermMeter)

type Column     = (VpermTable, RpermTable)

type Pinax      = Vector (Column)
type Syntagma   = Vector (Pinax)
type Arca       = Vector (Syntagma)

-- * Accessing the Data
-- ** By index
column :: Arca -> Int -> Int -> Int -> Column
column arca syntagma pinax col = arca ! syntagma ! pinax ! col

vperm :: Column -> Int -> VpermChoir
vperm col i = (fst col) ! i 

rperm :: Column -> Int -> Int -> Rperm
rperm col meter i = (snd col) ! meter ! i

-- ** By meaningful data
-- | Go straight to a voice and a rhythm permutation, given all the needed
-- variables and an index (which should be generated randomly). 
-- TODO random
getVperm :: Arca -> Style -> PenultLength -> Int -> Int -> VpermChoir
getVperm arca style penult sylCount i = vperm col i
    where
        col = column arca s p c
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2 -- check that this always works

getRperm :: Arca -> Style -> PenultLength -> Int -> Meter -> Int -> Rperm
getRperm arca style penult sylCount meter i = rperm col m i
    where
        col = column arca s p c
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2
        m = fromEnum meter

getVoice :: Arca -> Style -> PenultLength -> Int -> VoiceName 
    -> Int -> Vperm
getVoice arca style penult sylCount voice i = 
    getVperm arca style penult sylCount i ! fromEnum voice

getMusic :: Arca -> Style -> PenultLength -> Int -> 
    Meter -> VoiceName -> Int -> [(Int, Dur)]
getMusic arca style penult sylCount meter voice i =
    zip vpermVoice rperm 
        where
            vpermVoice = getVoice arca style penult sylCount voice i
            rperm = getRperm arca style penult sylCount meter i
-- TODO check for rests in getMusic

-- * Building the Ark

-- | Take a singly nested list and make it into a vector of vectors.
fromList2D :: [[a]] -> Vector (Vector (a))
fromList2D ls = fromList inner
    where
        inner = map (\ ls -> fromList ls) ls
