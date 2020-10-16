{-|
Module      : Aedifico
Description : Data structures for building Kircher's /Arca musarithmica/
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental


This module provides the data structures and methods for storing the data of
Kircher's ark and then extracting it. (*aedifico* = Latin, "I build")
The @Arca_musarithmica@ module actually builds it.

As described in Kircher's /Musurgia universalis/ (Rome, 1650), book 8, 
the ark is a box containing rods (/pinakes/), each of which includes columns
with voice and rhythm permutations. The rods are grouped according to style
into /syntagmata/, where /syntagma/ 1 is simple homorhythmic counterpoint.
There are two surviving exemplars of physical implementations of the ark.

The top part of Kircher's "rods" contain tables table of numbers with four
rows, where the numbers represent pitch offsets from a modal base note, and
the rows are the notes for the four voice parts SATB.  Each table represents
the notes to set a single phrase of text with a given number of syllables.

This module implements analogous data structures using Haskell types and
defines methods for building the ark from input data, and for accessing each
element of the ark data.

-}

module Aedifico where

import Data.Vector (Vector, (!), fromList)

-- * Data types

-- ** Equivalents of Kircher's Rods and Tables

-- | Pitches
--
-- The 'Pnum' is a 0-indexed diatonic pitch-class number, C through C an
-- octave higher. (In Kircher's 1-indexed system he uses both 1 and 8 for C so
-- we must be able to tell the difference.)
data Pnum = 
      PCc 
    | PCd 
    | PCe 
    | PCf 
    | PCg 
    | PCa 
    | PCb 
    | PCc8  -- ^ C an octave higher
    | Rest
    deriving (Show, Enum, Eq, Ord)

-- | Accidentals
data Accid = 
      Fl        -- ^ flat
    | Na        -- ^ natural
    | Sh        -- ^ sharp
    | AccidNil  -- ^ when note is a rest
    deriving (Show, Enum, Eq, Ord)

-- | Octaves
--
-- We set octave numbers in the Helmholtz system (middle C = C4); we only need
-- the enum 'OctNil' if the note is a rest.
-- 
-- __TODO__ check
data Octave = OctNil
    deriving (Show, Enum, Eq, Ord)

-- | Voices
--
-- The ark always produces four-voice polyphony.
data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Eq, Ord)

instance Show VoiceName where
    show Soprano = "soprano"
    show Alto    = "alto"
    show Tenor   = "tenor"
    show Bass    = "bass"

-- | Duration values
--
-- We use the mensural names; first the base values, then dotted variants,
-- then a series marked as rest values.
data Dur = 
      Br    -- ^ breve
    | Sb    -- ^ semibreve 
    | Mn    -- ^ minim
    | Sm    -- ^ semiminim
    | Fs    -- ^ fusa
    | BrD   -- ^ dotted breve
    | SbD   -- ^ dotted semibreve
    | MnD   -- ^ dotted minim
    | SmD   -- ^ dotted semiminim
    | FsD   -- ^ dotted fusa
    | BrR   -- ^ breve rest
    | SbR   -- ^ semibreve rest
    | MnR   -- ^ minim rest
    | SmR   -- ^ semiminim rest
    | FsR   -- ^ fusa rest
    deriving (Enum, Eq, Ord, Show)

-- | Metrical Systems
--
-- Kircher only seems to allow for duple (not making distinction between C and
-- cut C), cut C 3 (triple major) and C3 (triple minor).
--
-- __TODO__ Should we distinguish between C and cut C duple?
data Meter = Duple | TripleMajor | TripleMinor
    deriving (Enum, Eq, Ord, Show)

-- | Style
--
-- Kircher has a number of styles but we are so far only using simple
-- (note-against-note homorhythmic polyphony).
--
-- ___TODO___ implement other styles.
data Style = Simple | Fugal 
    deriving (Enum, Eq, Ord, Show)

-- | Penultimate Syllable Length
--
-- Every unit of text to be set to music must be marked with either a long or
-- short penultimate syllable.
data PenultLength = Long | Short 
    deriving (Enum, Eq, Ord)

instance Show PenultLength where
    show Long = "Long"
    show Short = "Short"

-- ** Elements of the ark

-- *** 'Vperm': Pitch combinations for four-voice choir

-- | The top part of Kircher's "rods" contain tables table of numbers with four rows,
-- where the numbers represent pitch offsets from a modal base note, and the
-- rows are the notes for the four voice parts SATB.
-- Each table represents the notes to set a single phrase of text with a given
-- number of syllables.
--
-- We implement the notes for one voice as a 'Vperm', a list of 'Int' values.
type Vperm      = [Int]

-- | A vector of four 'Vperm's makes a 'VpermChoir'.
type VpermChoir = Vector (Vperm)

-- | A Vector of 'VpermChoir's is a 'VpermTable', which represents the top
-- part of Kircher's "rods".
type VpermTable = Vector (VpermChoir)

-- *** 'Rperm': Rhythm permutations to match the 'Vperm'

-- | The bottom part of the "rods" contain tables of rhythmic values written
-- with musical notes. In the simple note-against-note style, there is one
-- list of values to match each table of voices.
--
-- We implement this using our 'Dur' data type for the rhythmic values.
-- An 'Rperm' is a list of 'Dur' values.
type Rperm      = [Dur]

-- | An 'RpermMeter' is a vector of 'Rperm's all in one meter (see the 'Meter'
-- data type above).
type RpermMeter = Vector (Rperm)

-- | The 'RpermTable' is a vector containing all the rhythmic permutations for
-- one of Kircher's "rods".
--
-- __TODO__: This implementation may not be sufficient for the more complex styles
-- where there are different rhythms for the four voices. Also, as noted
-- above, we may need to distinguish duple major and duple minor.
type RpermTable = Vector (RpermMeter)

-- ** Assembling the data into Kircher's structures

-- | The ark is a box containing rods (/pinakes/), each of which includes
-- columns with voice and rhythm permutations. The rods are grouped according
-- to style into /syntagmata/, where /syntagma/ 1 is simple homorhythmic
-- counterpoint.
--
-- We implement the 'Column' as a 2-tuple with one 'VpermTable' and one
-- 'RpermTable'. 
type Column     = (VpermTable, RpermTable)

-- | A vector of 'Column' instances is a 'Pinax'.
type Pinax      = Vector (Column)

-- | A vector of 'Pinax' instances is a 'Syntagma'.
type Syntagma   = Vector (Pinax)

-- | A vector of 'Syntagma' instances makes up the full 'Arca'.
type Arca       = Vector (Syntagma)

-- * Accessing the Data
-- ** By index

-- | Getting a 'Column' just requires indexing through nested vectors.
column :: Arca      -- ^ ark (there's only one, but someone could make more!)
        -> Int      -- ^ syntagma number
        -> Int      -- ^ pinax number
        -> Int      -- ^ column number
        -> Column
column arca syntagma pinax col = arca ! syntagma ! pinax ! col

-- | Getting a 'VpermChoir' means taking the first of the 'Column' 2-tuple; we
-- select which one using a random number (from @Fortuna@ module), though the
-- Inquisition forbids chance operations
vperm :: Column 
        -> Int          -- ^ Index of voice permutation within the column
        -> VpermChoir
vperm col i = (fst col) ! i 

-- | Getting an 'Rperm' means taking the second of the 'Column' 2-tuple, using
-- the meter and a random index (for Kircher, user's choice)
rperm :: Column 
        -> Meter      
        -> Int      -- ^ Index of rhythm permutation
        -> Rperm
rperm col meter i = (snd col) ! fromEnum meter ! i

-- ** By meaningful data

-- | The user of Kircher's arca needs only to know the number of syllables in
-- a phrase and whether the penultimate syllable is long or short. Then they
-- must freely (?) choose which table in the column.
--
-- We go straight to a voice and a rhythm permutation, given all the needed
-- variables and an index.
-- Instead of choosing freely we tempt fate and use a random number.
--
-- We subtract 2 from the number of syllables to get the column index, since
-- the first column in the /pinakes/ is for two-syllable words.
getVperm :: Arca 
            -> Style 
            -> PenultLength 
            -> Int          -- ^ syllable count
            -> Int          -- ^ (random) index
            -> VpermChoir
getVperm arca style penult sylCount i = vperm col i
    where
        col = column arca s p c
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2 -- check that this always works

-- | Select the rhythm values for a single phrase from the ark's rhythm
-- permutations (Rperms).
getRperm :: Arca 
            -> Style 
            -> PenultLength 
            -> Int      -- ^ syllable count
            -> Meter 
            -> Int      -- ^ (random) index
            -> Rperm
getRperm arca style penult sylCount meter i = rperm col meter i
    where
        col = column arca s p c
        s = fromEnum style
        p = fromEnum penult
        c = sylCount - 2

-- | Select the pitch numbers for a single voice from one of the ark's pitch
-- permutations ('Vperm's).
getVoice :: Arca 
            -> Style 
            -> PenultLength 
            -> Int          -- ^ syllable count
            -> VoiceName 
            -> Int          -- ^ (random) index
            -> Vperm
getVoice arca style penult sylCount voice i = 
    getVperm arca style penult sylCount i ! fromEnum voice


-- * Building the Ark

-- | To build the ark from the data in the @Arca/@ directory, we must take a
-- singly nested list and make it into a vector of vectors. This allows for
-- the data to be input and maintained more simply, as a nested list of
-- integers and strings, but then converted to vectors for better
-- performance.
fromList2D :: [[a]] -> Vector (Vector (a))
fromList2D ls = fromList inner
    where
        inner = map (\ ls -> fromList ls) ls
