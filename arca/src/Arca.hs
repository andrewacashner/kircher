{- |
 /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
-}

module Arca where
import Data.Vector (Vector, (!), fromList)

-- * Data types

-- ** Equivalents of Kircher's Rods and Tables

-- *** Enums
-- | 0-indexed diatonic pitch-class number
data Pnum = PCc | PCd | PCe | PCf | PCg | PCa | PCb | PCc8
    | Rest
    deriving (Show, Enum, Eq, Ord)

-- | Accidentals
data Accid = 
      Fl      -- flat
    | Na    -- natural
    | Sh    -- sharp
    | AccidNil   -- when note is a rest
    deriving (Show, Enum, Eq, Ord)

-- | Octaves
data Octave = OctNil
    deriving (Show, Enum, Eq, Ord)

-- | Voices
data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Eq, Ord)

instance Show VoiceName where
    show Soprano = "soprano"
    show Alto    = "alto"
    show Tenor   = "tenor"
    show Bass    = "bass"

-- | Duration values
data Dur = Br | Sb | Mn | Sm | Fs
    | BrD | SbD | MnD | SmD | FsD -- dotted
    | BrR | SbR | MnR | SmR | FsR -- rests
    deriving (Enum, Eq, Ord, Show)

-- | Metrical Systems
data Meter = Duple | TripleMajor | TripleMinor
    deriving (Enum, Eq, Ord, Show)

data Style = Simple | Fugal 
    deriving (Enum, Eq, Ord, Show)

data PenultLength = Long | Short 
    deriving (Enum, Eq, Ord)

instance Show PenultLength where
    show Long = "Long"
    show Short = "Short"

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
getVperm :: Arca -> Style -> PenultLength -> Int -> Int -> VpermChoir
getVperm arca style penult sylCount i = vperm col i
    where
        col = column arca s p c
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2 -- check that this always works

-- | Select the rhythm values for a single phrase from the ark's rhythm
-- permutations (Rperms).
getRperm :: Arca -> Style -> PenultLength -> Int -> Meter -> Int -> Rperm
getRperm arca style penult sylCount meter i = rperm col m i
    where
        col = column arca s p c
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2
        m = fromEnum meter

-- | Select the pitch numbers for a single voice from one of the ark's pitch
-- permutations (Vperms).
getVoice :: Arca -> Style -> PenultLength -> Int -> VoiceName 
    -> Int -> Vperm
getVoice arca style penult sylCount voice i = 
    getVperm arca style penult sylCount i ! fromEnum voice

-- * Building the Ark

-- | Take a singly nested list and make it into a vector of vectors. This
-- allows for the data to be input and maintained more simply, as a nested
-- list of integers and strings, but then converted to vectors for better
-- performance.
fromList2D :: [[a]] -> Vector (Vector (a))
fromList2D ls = fromList inner
    where
        inner = map (\ ls -> fromList ls) ls
