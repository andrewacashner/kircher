{- |
 /Arca musarithmica Athanasii Kircheri Societatis Iesu MDCL./
 
 This module implements Kircher's ark by building the data structure
 containing all the necessary information for automated composition.

-}

module Arca where
import Data.Vector (Vector, (!), fromList)

-- * Data types

-- ** Equivalents of Kircher's Rods and Tables

-- *** Enums
-- We set up the pitches, accidentals, voice names, and durations as data
-- types intelligible to the programmer.

-- **** Pitches
-- | 0-indexed diatonic pitch-class number, C through C an octave higher
-- (In Kircher's 1-indexed system he uses both 1 and 8 for C so we must be
-- able to tell the difference.)
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

-- **** Accidentals
data Accid = 
      Fl        -- ^ flat
    | Na        -- ^ natural
    | Sh        -- ^ sharp
    | AccidNil  -- ^ when note is a rest
    deriving (Show, Enum, Eq, Ord)

-- **** Octaves
data Octave = OctNil
    deriving (Show, Enum, Eq, Ord)

-- **** Voices
-- | The ark always produces four-voice polyphony.
data VoiceName = Soprano | Alto | Tenor | Bass
    deriving (Enum, Eq, Ord)

instance Show VoiceName where
    show Soprano = "soprano"
    show Alto    = "alto"
    show Tenor   = "tenor"
    show Bass    = "bass"

-- **** Duration values
-- Using mensural names; base values, then dotted variants, then a series
-- marked as rest values
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

-- **** Metrical Systems
-- | Kircher only seems to allow for duple (not making distinction between C and
-- cut C), cut C 3 (triple major) and C3 (triple minor).
--
-- TODO Should we distinguish between C and cut C duple?
data Meter = Duple | TripleMajor | TripleMinor
    deriving (Enum, Eq, Ord, Show)

-- **** Style
-- | Kircher has a number of styles but we are so far only using simple
-- (note-against-note homorhythmic polyphony).
--
-- TODO implement other styles.
data Style = Simple | Fugal 
    deriving (Enum, Eq, Ord, Show)

-- **** Penultimate Syllable Length
-- | Every unit of text to be set to music must be marked with either a long
-- or short penultimate syllable.
data PenultLength = Long | Short 
    deriving (Enum, Eq, Ord)

instance Show PenultLength where
    show Long = "Long"
    show Short = "Short"

-- ** Elements of the ark

-- *** @Vperm@ -- pitch combinations for four-voice choir

-- | The top part of Kircher's "rods" contain tables table of numbers with four rows,
-- where the numbers represent pitch offsets from a modal base note, and the
-- rows are the notes for the four voice parts SATB.
-- Each table represents the notes to set a single phrase of text with a given
-- number of syllables.
--
-- We implement the notes for one voice as a @Vperm@, a list of integers.
-- A vector of four @Vperm@s makes a @VpermChoir@, and a vector of those is a
-- @VpermTable@, which represents the top part of Kircher's "rods".
type Vperm      = [Int]
type VpermChoir = Vector (Vperm)
type VpermTable = Vector (VpermChoir)

-- *** @Rperm@ -- rhythm permutations to match the @Vperm@
-- | The bottom part of the "rods" contain tables of rhythmic values written
-- with musical notes. In the simple note-against-note style, there is one
-- list of values to match each table of voices.
--
-- We implement this using our @Dur@ data type for the rhythmic values.
-- An @Rperm@ is a list of @Dur@ values.
-- An @RpermMeter@ is a vector of @Rperm@s all in one meter (see the 'Meter'
-- data type above).
-- The @RpermTable@ is a vector containing all the rhythmic permutations for
-- one of Kircher's "rods".
--
-- TODO: This implementation may not be sufficient for the more complex styles
-- where there are different rhythms for the four voices. Also, as noted
-- above, we may need to distinguish duple major and duple minor.
type Rperm      = [Dur]
type RpermMeter = Vector (Rperm)
type RpermTable = Vector (RpermMeter)

-- ** Assembling the data into Kircher's structures
-- | The ark is a box containing rods (/pinakes/), each of which includes
-- columns with voice and rhythm permutations. The rods are grouped according
-- to style into /syntagmata/, where /syntagma/ 1 is simple homorhythmic
-- counterpoint.
--
-- We implement the @Column@ as a 2-tuple with one @VpermTable@ and one
-- @RpermTable@. A vector of @Column@ instances is a @Pinax@, a vector of
-- @Pinax@ instances is a @Syntagma@, and a vector of @Syntagma@ instances
-- makes up the full @Arca@.
type Column     = (VpermTable, RpermTable)

type Pinax      = Vector (Column)
type Syntagma   = Vector (Pinax)
type Arca       = Vector (Syntagma)

-- * Accessing the Data
-- ** By index
-- | Getting a 'Column' just requires indexing through nested vectors.
column :: Arca -> Int -> Int -> Int -> Column
column arca syntagma pinax col = arca ! syntagma ! pinax ! col

-- | Getting a 'VpermChoir' means taking the first of the @Column@ 2-tuple
vperm :: Column -> Int -> VpermChoir
vperm col i = (fst col) ! i 

-- | Getting an 'Rperm' means taking the second of the @Column@ 2-tuple
rperm :: Column -> Int -> Int -> Rperm
rperm col meter i = (snd col) ! meter ! i

-- ** By meaningful data
-- | The user of Kircher's arca needs only to know the number of syllables in
-- a phrase and whether the penultimate syllable is long or short. Then they
-- must freely (?) choose which table in the column.
--
-- We go straight to a voice and a rhythm permutation, given all the needed
-- variables and an index.
-- Instead of choosing freely we tempt fate and use a random number.
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
