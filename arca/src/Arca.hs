{- |
 /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
-}

module Arca where
import Data.Vector (Vector, (!), fromList)

-- * Data types

-- ** Equivalents of Kircher's Rods and Tables

-- *** Duration values
data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Eq, Ord, Show)

data Dur = Br | Sb | Mn | Sm | Fs
    | BrD | SbD | MnD | SmD | FsD -- dotted
    | BrR | SbR | MnR | SmR | FsR -- rests
    deriving (Enum, Eq, Ord, Show)

data Meter = Duple | TripleMajor | TripleMinor
    deriving (Enum, Eq, Ord, Show)

data Style = Simple | Fugal 
    deriving (Enum, Eq, Ord, Show)

data PenultLength = Long | Short 
    deriving (Enum, Eq, Ord, Show)

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

-- *** Get music data for a single voice

-- | Central function of the ark: given all parameters required by Kircher
-- (style, meter, syllable count, penultimate syllable length), select a voice
-- permutation (Kircher's number tables) from the appropriate part of the ark
-- and match it to a rhythm permutation (his tables of note values).
-- Return a list of pairs, each contain a pitch number and a duration, e.g.
-- @[(5,Sb),(5,Mn)]@
getMusic :: Arca -> Style -> PenultLength -> Int -> 
    Meter -> VoiceName -> Int -> [(Int, Dur)]
getMusic arca style penult sylCount meter voice i =
    zip vpermVoice rperm 
        where
            vpermVoice = getVoice arca style penult sylCount voice i
            rperm = getRperm arca style penult sylCount meter i
-- TODO check for rests in getMusic

{-
 - if rperm[i] is a rest rhythm type, then join it to a rest;
 - advance to rperm[i+1] but stay with vperm[i] for next pair
 -}

-- | Check to see if a rhythmic duration is a rest type
isRest :: Dur -> Bool
isRest dur = dur >= BrR 

-- | Take two lists and zip them together, that is, make a new list of ordered
-- pairs constructed from sequential items of the two lists, BUT:
-- if an element in the first list satisfies a given @test@, make a pair of that
-- element and a sustitute element (@sub@) instead of the corresponding
-- element from the second list.
zipFill [] _ test sub = [] 
    -- stop when the first list is done
zipFill a [] test sub = zipFill a [sub] test sub
    -- when the second list is done, use @sub@ as placeholder for remaining
    -- items in first list
zipFill (a:as) (b:bs) test sub = 
    if test a 
        then (a, b) : zipFill as bs test sub 
        else (a, sub) : zipFill as (b:bs) test sub
    -- build a list of pairs of either the heads of both lists or the head
    -- of the first list and the @sub@ value

-- TODO
-- build a list of Pitch or Rest types using something like zipFill with
-- isRest as the test. But instead of just making pairs, create Pitch or Rest
-- types. How to have it return either kind?

-- * Building the Ark

-- | Take a singly nested list and make it into a vector of vectors. This
-- allows for the data to be input and maintained more simply, as a nested
-- list of integers and strings, but then converted to vectors for better
-- performance.
fromList2D :: [[a]] -> Vector (Vector (a))
fromList2D ls = fromList inner
    where
        inner = map (\ ls -> fromList ls) ls
