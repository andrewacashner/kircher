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

It also defines the data types needed for the other modules.
__TODO__: Should it?

-}

module Aedifico where

import Data.Vector 
    (Vector, 
     (!), 
     fromList)

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
      FlFl      -- ^ double flat
    | Fl        -- ^ flat
    | Na        -- ^ natural
    | Sh        -- ^ sharp
    | ShSh      -- ^ double sharp
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

-- | Vocal Ranges
type VoiceRanges = [(Pitch, Pitch)]

-- | Duration values
--
-- We use the mensural names; first the base values, then dotted variants,
-- then a series marked as rest values.
data Dur = 
      DurNil -- ^ unset
    | Br    -- ^ breve
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

-- | A 'Pitch' stores the essential information for notating a single note.
data Pitch = Pitch {
    pnum  :: Pnum, -- ^ Enum for diatonic pitch number
    oct   :: Int,  -- ^ Helmholtz system, middle C = 4
    dur   :: Dur,  -- ^ Duration, one of @Dur@ enum
    accid :: Accid -- ^ Accidental
} deriving (Show, Eq, Ord)


-- | Metrical Systems
--
-- Kircher only seems to allow for duple (not making distinction between C and
-- cut C), cut C 3 (triple major) and C3 (triple minor).
--
-- __TODO__ Should we distinguish between C and cut C duple?
data MusicMeter = Duple | TripleMajor | TripleMinor
    deriving (Enum, Eq, Ord)

instance Show MusicMeter where
    show meter = case meter of 
        Duple       -> "Duple"
        TripleMajor -> "TripleMajor"
        TripleMinor -> "TripleMinor"

-- | Select meter by string
toMusicMeter :: String -> MusicMeter
toMusicMeter s = case s of 
    "Duple"         -> Duple
    "TripleMajor"   -> TripleMajor
    "TripleMinor"   -> TripleMinor

-- | Text meter (of input text, distinguished from musical meter of setting)
data TextMeter =  Prose     -- ^ No meter, free, or irregular
                | Adonius
                | Dactylus
                deriving (Show, Enum, Eq, Ord)

-- | Select text meter by string
toTextMeter :: String -> TextMeter
toTextMeter s = case s of
    "Prose"     -> Prose
    "Adonius"   -> Adonius
    "Dactylus"  -> Dactylus

-- | Style
--
-- Kircher has a number of styles but we are so far only using simple
-- (note-against-note homorhythmic polyphony).
--
-- ___TODO___ implement other styles.
data Style = Simple | Fugal 
    deriving (Enum, Eq, Ord)

instance Show Style where
    show style = case style of 
        Simple -> "Simple"
        Fugal  -> "Fugal"

-- | Select style by string
toStyle :: String -> Style
toStyle s = case s of
    "Simple"    -> Simple
    "Fugal"     -> Fugal

-- | Mode
--
-- Kircher's table of modes is different from the traditional chant modes.
-- They are more like "church keys" or /toni/ for psalm intonations.
data Mode = Mode1 | Mode2 | Mode3 | Mode4 | Mode5 | Mode6 
            | Mode7 | Mode8 | Mode9 | Mode10 | Mode11 | Mode12
    deriving (Enum, Eq, Ord, Show)

-- | Select mode by string
toMode :: String -> Mode
toMode s = case s of
    "Mode1" -> Mode1
    "Mode2" -> Mode2
    "Mode3" -> Mode3
    "Mode4" -> Mode4
    "Mode5" -> Mode5
    "Mode6" -> Mode6
    "Mode7" -> Mode7
    "Mode8" -> Mode8
    "Mode9" -> Mode9
    "Mode10" -> Mode10
    "Mode11" -> Mode11
    "Mode12" -> Mode12

-- ** Kircher's table with the mode systems and mode notes, on the lid of the
-- arca. We include this in the main `Arca`.  

-- | Mode system, /durus/ (natural)
-- or /mollis/ (one flat in the key signature)
data System = Durus | Mollis
    deriving (Enum, Eq, Ord)

-- | The series of 'System' values for the modes
type ModeSystem = Vector (System)

-- | Combination 'Pnum' and 'Accid' used to set a Pitch
type PnumAccid = (Pnum, Accid)

-- | A list of scales, including some notes with accidentals, from Kircher 
type ModeList = Vector (Vector (PnumAccid))


-- TODO Kircher's mode mixtures for each
-- TODO Kircher's mood/character for each


-- | Penultimate Syllable Length
--
-- Every unit of text to be set to music must be marked with either a long or
-- short penultimate syllable.
data PenultLength = Long | Short 
    deriving (Enum, Eq, Ord)

instance Show PenultLength where
    show Long = "Long"
    show Short = "Short"

-- | Pinakes 
data Pinax =  Pinax1
            | Pinax2
            | Pinax3a
            | Pinax3b 
            deriving (Show, Enum, Ord, Eq)

-- | All the ark settings in one structure: We use this to pass configuration
-- settings through many functions down to the core level of pulling data from
-- the ark.
data ArkConfig = ArkConfig {
    arkStyle :: Style,
    arkMode  :: Mode,
    arkMusicMeter :: MusicMeter,
    arkTextMeter  :: LyricMeter
} deriving (Eq, Ord)

instance Show ArkConfig where
    show config = 
        "style: "   ++ (show $ arkStyle config) ++ 
        ", meter: " ++ (show $ arkMusicMeter config) ++ 
        ", mode: "  ++ (show $ (fromEnum $ arkMode config) + 1) ++ " "

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

-- | An 'RpermMeter' includes a vector of 'Rperm's all in one meter (see the 'MusicMeter'
-- data type above) and the length of that vector.
--
-- Kircher has a variable number of 'Rperm's in the different meters, in each
-- column, so we need to know how many there are.
data RpermMeter = RpermMeter {
    rpermMax :: Int,            -- ^ length of 'rperms'
    rperms :: Vector (Rperm)
}

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
data Arca = Arca {
    perms   :: Vector (Syntagma),
    modes   :: ModeList,
    systems :: ModeSystem,
    ranges  :: VoiceRanges
}

-- * Accessing the Data
-- ** By index

-- | Getting a 'Column' just requires indexing through nested vectors.
column :: Arca      -- ^ ark (there's only one, but someone could make more!)
        -> Int      -- ^ syntagma number
        -> Pinax    -- ^ pinax enum 
        -> Int      -- ^ column number
        -> Column
column arca syntagma pinax col = (perms arca) ! syntagma ! (fromEnum pinax) ! col

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
        -> MusicMeter      
        -> Int      -- ^ Index of rhythm permutation
        -> Rperm
rperm col meter i = rperms rpermTable ! index
        where
            index = i `mod` rpermMax rpermTable
            rpermTable = (snd col) ! fromEnum meter

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
-- 
-- __TODO__: Does @columnIndex - 2@ always work?
getVperm :: Arca 
            -> ArkConfig    -- ^ we need 'Style'
            -> PenultLength -- ^ 'Long' or 'Short' 
            -> Int          -- ^ syllable count
            -> Int          -- ^ (random) index
            -> VpermChoir
getVperm arca config penult sylCount i = vperm col i
    where
        pinax = case (arkTextMeter config) 
            Prose    -> if penult == Short 
                        then Pinax1
                        else Pinax2
            Adonus   -> Pinax3a
            Dactylus -> Pinax3b

        col          = column arca style pinax columnIndex
        style        = fromEnum (arkStyle config)
        penultLength = fromEnum penult
        columnIndex  = sylCount - 2

-- | Select the rhythm values for a single phrase from the ark's rhythm
-- permutations (Rperms).
getRperm :: Arca 
            -> ArkConfig    -- ^ we need 'Style' and 'MusicMeter' 
            -> PenultLength 
            -> Int          -- ^ syllable count
            -> Int          -- ^ (random) index
            -> Rperm
getRperm arca config penult sylCount i = rperm col meter i 
    where
        meter        = arkMusicMeter config
        col          = column arca style penultLength columnIndex
        style        = fromEnum (arkStyle config)
        penultLength = fromEnum penult
        columnIndex  = sylCount - 2

-- | Select the pitch numbers for a single voice from one of the ark's pitch
-- permutations ('Vperm's).
getVoice :: Arca 
            -> ArkConfig    -- ^ we pass this along to 'getVperm'
            -> PenultLength 
            -> Int          -- ^ syllable count
            -> VoiceName 
            -> Int          -- ^ (random) index
            -> Vperm
getVoice arca config penult sylCount voice i = 
    getVperm arca config penult sylCount i ! fromEnum voice


-- * Building the Ark

-- | To build the ark from the data in the @Arca/@ directory, we must take a
-- singly nested list and make it into a vector of vectors. This allows for
-- the data to be input and maintained more simply, as a nested list of
-- integers and strings, but then converted to vectors for better
-- performance.
-- The innermost layer stays in list format.
--
-- __TODO__: Optimize?
fromList2D :: [[a]] -> Vector (Vector (a))
fromList2D ls = fromList inner
    where
        inner = map fromList ls

-- | Application of 'fromList2D' to 'Vperm'
buildVpermTable :: [[Vperm]] -> VpermTable
buildVpermTable ls = fromList2D ls

-- | Make a new 'RpermMeter' that knows its own length.
newRpermMeter :: [Rperm] -> RpermMeter
newRpermMeter theseRperms = RpermMeter {
    rpermMax = length theseRperms,
    rperms   = fromList theseRperms
}

-- | Build an 'RpermTable' with 'RpermMeter's that know their length.
buildRpermTable :: [[Rperm]] -> RpermTable
buildRpermTable ls = fromList $ map newRpermMeter ls


