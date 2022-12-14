{-|
Module      : Aedifico
Description : Data structures for building Kircher's /Arca musarithmica/
Copyright   : (c) 2022 Andrew A. Cashner
Stability   : Experimental


This module provides the data structures and methods for storing the data of
Kircher's ark and then extracting it. (*aedifico* = Latin, "I build")
The @Arca_musarithmica@ module actually builds it.

= Kircher's specification

As described in Kircher's /Musurgia universalis/ (Rome, 1650), book 8, 
the ark is a box containing rods (/pinakes/), each of which includes columns
with voice and rhythm permutations. The rods are grouped according to style
into /syntagmata/, where /syntagma/ 1 is simple homorhythmic counterpoint.
There are two surviving exemplars of physical implementations of the ark.

The top part of Kircher's "rods" contain tables table of numbers with four
rows, where the numbers represent pitch offsets from a modal base note, and
the rows are the notes for the four voice parts SATB.  Each table represents
the notes to set a single phrase of text with a given number of syllables.

= Implementation

This module implements analogous data structures using Haskell types and
defines methods for building the ark from input data, and for accessing each
element of the ark data. 

It also defines the data types needed for the other modules.

== Structure of the ark in Haskell implementation (simplified)

>    Arca
>        vperms
>            Arca                     = Vector (Syntagma)
>            Syntagma                 = Vector (Pinax)
>            Pinax                    = Vector (Column)
>            Column { colVpermTable } = VpermTable
>            VpermTable { vperms }    = Vector (VpermChoir)
>            VpermChoir               = Vector (Vperm)
>            Vperm                    = [Int]
>
>        rperms
>            Arca                     = Vector (Syntagma)
>            Syntagma                 = Vector (Pinax)
>            Pinax                    = Vector (Column)
>            Column { colRpermTable } = RpermTable
>            RpermTable               = Vector (RpermMeter)
>            RpermMeter { rperms }    = Vector (RpermChoir)
>            RpermChoir               = Vector (Rperm)
>            Rperm                    = [Dur]

=== Accessing perms directly

The test module @Spec.hs@ shows how to access all of the ark data directly.
These notes might clarify how to reach individual ark vperms or rperms.

>       vperms
>            perms arca          :: Vector (Vector (Vector Column))
>            colVpermTable       :: VpermTable
>            vperms vpermTable   :: Vector (Vector [Int])
>
>            vperm :: [Int]
>            vperm = vperms table ! vpermIndex ! voiceIndex
>            where
>                table  = colVpermTable $ column ! columnIndex
>                column = perms arca ! syntagmaIndex ! pinaxIndex ! columnIndex
>
>       rperms
>            rperm :: [Dur]
>            rperm = rperms table ! rpermMeterIndex ! rpermVoiceIndex
>            where
>                table  = colVpermTable $ column ! columnIndex
>                column = perms arca ! syntagmaIndex ! pinaxIndex ! columnIndex
-}

module Aedifico where

import Data.Maybe
    ( Maybe
    , isNothing
    , fromJust
    )

import Data.Vector 
    ( Vector
    , (!?)
    , fromList
    )

import Data.AssocList.List.Concept
    (AssocList)

import Data.AssocList.List.Eq
    (lookupFirst)

-- * Utitilies

-- | Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
as !!? i | i >= length as = Nothing
        | otherwise      = Just $ as !! i


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

-- | Convert any integer to a 'Pnum'
toPnum :: Int -> Pnum
toPnum n = toEnum $ (n - 1) `mod` 7

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
data VoiceName = Cantus | Alto | Tenor | Bass
    deriving (Enum, Eq, Ord)

instance Show VoiceName where
    show Cantus = "Cantus"
    show Alto    = "Alto"
    show Tenor   = "Tenor"
    show Bass    = "Bass"

-- | Vocal Ranges
data VoiceRange  = VoiceRange {
    low :: Pitch,
    high :: Pitch
} deriving (Show, Eq, Ord)

-- | Set of 'VoiceRange' data for each 'VoiceName'
data VoiceRanges = VoiceRanges {
    cantusRange :: VoiceRange,
    altoRange    :: VoiceRange,
    tenorRange   :: VoiceRange,
    bassRange    :: VoiceRange
}

-- | Access data from 'VoiceRanges' by 'VoiceName'
getRange :: VoiceName -> VoiceRanges -> VoiceRange
getRange name ranges = selector ranges
    where 
        selector = case name of
            Cantus  -> cantusRange 
            Alto    -> altoRange 
            Tenor   -> tenorRange 
            Bass    -> bassRange 



-- | Duration values
--
-- We use the mensural names; first the base values, then dotted variants,
-- then a series marked as rest values.
data Dur = 
      DurNil -- ^ unset
    | Lg    -- ^ longa
    | Br    -- ^ breve
    | Sb    -- ^ semibreve 
    | Mn    -- ^ minim
    | Sm    -- ^ semiminim
    | Fs    -- ^ fusa
    | LgD   -- ^ dotted longa
    | BrD   -- ^ dotted breve
    | SbD   -- ^ dotted semibreve
    | MnD   -- ^ dotted minim
    | SmD   -- ^ dotted semiminim
    | FsD   -- ^ dotted fusa
    | LgR   -- ^ longa rest
    | BrR   -- ^ breve rest
    | SbR   -- ^ semibreve rest
    | MnR   -- ^ minim rest
    | SmR   -- ^ semiminim rest
    | FsR   -- ^ fusa rest
    deriving (Enum, Eq, Ord, Show)

-- | How should the accidental be displayed? (Needed for MEI)
data AccidType = None       -- ^ No accidental
               | Written    -- ^ MEI accid 
               | Implicit   -- ^ MEI accid.ges
               | Suggested  -- ^ MEI accid + func="edit"
               deriving (Show, Eq, Ord)


-- | A 'Pitch' stores the essential information for notating a single note.
data Pitch = Pitch {
    pnum  :: Pnum,          -- ^ Enum for diatonic pitch number
    oct   :: Int,           -- ^ Helmholtz system, middle C = 4
    dur   :: Dur,           -- ^ Duration, one of @Dur@ enum
    accid :: Accid,         -- ^ Accidental
    accidType :: AccidType  -- ^ Type of accidental for display
} deriving (Show, Eq, Ord)

-- | Make a pitch with only 'pnum' and octave, no duration or accidental
simplePitch :: (Pnum, Int)  -- ^ Pitch enum and Helmholtz octave number
            -> Pitch
simplePitch (p, o) = Pitch {
    pnum      = p,
    oct       = o,
    dur       = DurNil,
    accid     = Na,
    accidType = None
}
-- *** Metrical Systems

-- | Kircher only seems to allow for duple (not making distinction between C and
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
    _ -> error $ "bad meter"

-- *** Textual/poetic meter

-- | Text meter (of input text, distinguished from musical meter of setting)
data TextMeter = 
      TextMeterNil
    | Prose                         -- ^ No meter, free, or irregular
    | ProseLong                     -- ^ Prose, 2-6 syllabels, penultimate is long
    | ProseShort                    -- ^ Prose, 2-6 syllables, penultimate is short
    | Adonium                       -- ^ 5  syllables (@`--`-@)
    | Dactylicum                    -- ^ 6  syllables (@`--`--@)
    | IambicumEuripidaeum           -- ^ 6  syllables (@`-`-`-@)
    | Anacreonticum                 -- ^ 7  syllables, penultimate long 
    | IambicumArchilochicum         -- ^ 8  syllables, penultimate short
    | IambicumEnneasyllabicum       -- ^ 9  syllables, penultimate long
    | Enneasyllabicum               -- ^ 9  syllables (generic)
    | Decasyllabicum                -- ^ 10 syllables, penultimate short
    | PhaleuciumHendecasyllabicum   -- ^ 11 syllables
    | Hendecasyllabicum             -- ^ 11 syllables (generic)
    | Sapphicum                     -- ^ 11 syllables, three lines + 5-syllable tag
    | Dodecasyllabicum              -- ^ 12 syllables, penultimate short
    deriving (Show, Enum, Eq, Ord)

-- | Select text meter by string
toTextMeter :: String -> TextMeter
toTextMeter s = case s of
    "Prose"                       -> Prose
    "ProseLong"                   -> ProseLong
    "ProseShort"                  -> ProseShort
    "Adonium"                     -> Adonium
    "Dactylicum"                  -> Dactylicum
    "IambicumEuripidaeum"         -> IambicumEuripidaeum
    "Anacreonticum"               -> Anacreonticum
    "IambicumArchilochicum"       -> IambicumArchilochicum 
    "IambicumEnneasyllabicum"     -> IambicumEnneasyllabicum
    "Enneasyllabicum"             -> Enneasyllabicum
    "Decasyllabicum"              -> Decasyllabicum
    "PhaleuciumHendecasyllabicum" -> PhaleuciumHendecasyllabicum 
    "Hendecasyllabicum"           -> Hendecasyllabicum
    "Sapphicum"                   -> Sapphicum
    "Dodecasyllabicum"            -> Dodecasyllabicum
    _ -> error $ unwords ["Unknown textmeter", s]

-- | Get maximum number of syllables for a TextMeter
maxSyllables :: TextMeter -> Int
maxSyllables meter = case meter of
    Prose                       -> 6
    Adonium                     -> 5
    Dactylicum                  -> 6
    IambicumEuripidaeum         -> 6
    Anacreonticum               -> 7
    IambicumArchilochicum       -> 8 
    IambicumEnneasyllabicum     -> 9
    Enneasyllabicum             -> 9
    Decasyllabicum              -> 10
    PhaleuciumHendecasyllabicum -> 11 
    Hendecasyllabicum           -> 11
    Sapphicum                   -> 11
    Dodecasyllabicum            -> 12
    _ -> error "bad meter"


-- *** Style

-- | The choice of style determines which of Kircher's three /syntagmata/ we
-- select. 'Simple' style calls up Syntagma 1 for simple, note-against-note
-- (first-species) homorhythmic counterpoint. 'Florid' style calls up Syntagma
-- 2 for syllabic, imitative, and even in some permutations fugal
-- counterpoint. 
--
-- __TODO__ There is also a third syntagma, for adding rhetorical figures to
-- simple counterpoint for more nuanced text-setting. We have not yet
-- implemented this, and do not know if it can be fully automated.
data Style =  Simple -- ^ Syllabic, homorhythmic counterpoint (syntagma 1)
            | Florid -- ^ Melismatic, imitative counterpoint (syntagma 2)
    deriving (Show, Enum, Eq, Ord)

-- | Select style by string (used in processing XML input)
toStyle :: String -- ^ "Simple" or "Florid" 
        -> Style
toStyle s = case s of
    "Simple"    -> Simple
    "Florid"    -> Florid
    _           -> error $ unwords ["Unknown style", s]

-- | Tone
--
-- Kircher's table of tones is a hybrid of /toni ecclesiastici/ or "church
-- keys" which were matched to the eight traditional psalm tones in Gregorian
-- chant, and the twelve modes of Zarlino.
data Tone = Tone1 | Tone2 | Tone3 | Tone4 | Tone5 | Tone6 
            | Tone7 | Tone8 | Tone9 | Tone10 | Tone11 | Tone12 | ToneUnset
    deriving (Show, Enum, Eq, Ord)

-- | Select tone by string (e.g., "Tone1" or "Tone12" in XML input)
toTone :: String -> Tone
toTone s = case s of
    "Tone1" -> Tone1
    "Tone2" -> Tone2
    "Tone3" -> Tone3
    "Tone4" -> Tone4
    "Tone5" -> Tone5
    "Tone6" -> Tone6
    "Tone7" -> Tone7
    "Tone8" -> Tone8
    "Tone9" -> Tone9
    "Tone10" -> Tone10
    "Tone11" -> Tone11
    "Tone12" -> Tone12
    _ -> error $ unwords ["Unknown tone", s]

-- ** Kircher's table with the tone systems and tone notes, on the lid of the
-- arca. We include this in the main @Arca@.  

-- | Tone system, /durus/ (natural)
-- or /mollis/ (one flat in the key signature)
data System = Durus | Mollis
    deriving (Enum, Eq, Ord)

-- | The series of 'System' values for the tones
type ToneSystem = Vector (System)

-- | Combination 'Pnum' and 'Accid' used to set a Pitch
type PnumAccid = (Pnum, Accid)

-- | A list of scales, including some notes with accidentals, from Kircher 
type ToneList = Vector (Vector PnumAccid)

-- | List of tones appropriate for a single pinax
type PinaxLegalTones = AssocList PinaxLabel [[Tone]]

-- | List of tones appropriate for each pinax within each syntagma (style):
-- association list mapping style to sets of /pinakes/, and then /pinakes/ to
-- tones
type PinaxToneList = AssocList Style PinaxLegalTones

-- | Lookup a value by equality in an association list, or raise an error if
-- not found
assocLookup :: Eq a => a -> AssocList a b -> String -> b
assocLookup key list msg
    | isNothing found = error msg
    | otherwise = fromJust found
    where found = lookupFirst key list

-- | Get a list of legal tones for a given 'Style' and 'PinaxLabel'
tonesPerStyle :: Style -> PinaxLabel -> PinaxToneList -> [[Tone]]
tonesPerStyle s p = tonesPerPinax p . pinakesPerStyle s
    where
        tonesPerPinax :: PinaxLabel -> PinaxLegalTones -> [[Tone]]
        tonesPerPinax p ls = assocLookup p ls 
                                "pinax not found in list of legal tones"

        pinakesPerStyle :: Style -> PinaxToneList -> PinaxLegalTones
        pinakesPerStyle s ls = assocLookup s ls 
                                "syntagma not found in list of legal tones"

-- TODO Kircher's tone mixtures for each
-- TODO Kircher's mood/character for each


-- | Penultimate Syllable Length
--
-- Every unit of text to be set to music must be marked with either a long or
-- short penultimate syllable.
data PenultLength = Long | Short 
    deriving (Show, Enum, Eq, Ord)

-- | 'Pinax' maps to 'TextMeter'
data PinaxLabel =  
      Pinax1 
    | Pinax2
    | Pinax3
    | Pinax3a
    | Pinax3b
    | Pinax4
    | Pinax5
    | Pinax6
    | Pinax7
    | Pinax8
    | Pinax9
    | Pinax10
    | Pinax11
    | PinaxNil
    deriving (Show, Ord, Eq)

-- | Extract a 'Pinax' from the ark by style and pinax label
arca2pinax :: Arca -> Style -> PinaxLabel -> Pinax
arca2pinax arca style pinaxLabel = pinax
    where
        pinax    = getVectorItem "arca2pinax:pinax" syntagma pIndex
        syntagma = getVectorItem "arca2pinax:syntagma" (perms arca) $ fromEnum style

        pIndex = case style of
            Simple -> simplePinax
            Florid -> floridPinax

        simplePinax = case pinaxLabel of
            Pinax1  -> 0
            Pinax2  -> 1
            Pinax3a -> 2
            Pinax3b -> 3
            Pinax4  -> 4
            Pinax5  -> 5
            Pinax6  -> 6
            Pinax7  -> 7
            Pinax8  -> 8
            Pinax9  -> 9
            Pinax10 -> 10
            Pinax11 -> 11
            
        floridPinax = case pinaxLabel of
            Pinax1  -> 0
            Pinax2  -> 1
            Pinax3  -> 2
            Pinax4  -> 3
            Pinax5  -> 4
            Pinax6  -> 5


-- | Get pinax from textual meter; this depends on the 'Style' because the
-- /syntagmata/ differ in the order of meters, so 'IambicumEuripidaeum' meter
-- in Syntagma 1 is 'Pinax4', but in Syntagma 2 it is 'Pinax2'.
meter2pinax :: Style -> TextMeter -> PinaxLabel
meter2pinax s m = case s of
        Simple -> meter2pinaxSimple m
        Florid -> meter2pinaxFlorid m

        where
            meter2pinaxSimple m = case m of
                Prose       -> error "Need to determine ProseShort or ProseLong"
                ProseLong                   -> Pinax1
                ProseShort                  -> Pinax2
                Adonium                     -> Pinax3a
                Dactylicum                  -> Pinax3b
                IambicumEuripidaeum         -> Pinax4
                Anacreonticum               -> Pinax5
                IambicumArchilochicum       -> Pinax6
                IambicumEnneasyllabicum     -> Pinax7
                Enneasyllabicum             -> Pinax7
                Decasyllabicum              -> Pinax8
                PhaleuciumHendecasyllabicum -> Pinax9
                Hendecasyllabicum           -> Pinax9
                Sapphicum                   -> Pinax10
                Dodecasyllabicum            -> Pinax11
                _ -> error $ unwords ["bad textMeter", show m]

            meter2pinaxFlorid m = case m of
                Adonium                     -> Pinax1
                Dactylicum                  -> Pinax1
                IambicumEuripidaeum         -> Pinax2
                Anacreonticum               -> Pinax3
                IambicumArchilochicum       -> Pinax4
                IambicumEnneasyllabicum     -> Pinax5
                Enneasyllabicum             -> Pinax5
                Decasyllabicum              -> Pinax5
                PhaleuciumHendecasyllabicum -> Pinax6
                Hendecasyllabicum           -> Pinax6
                Sapphicum                   -> Pinax6
                _ -> error $ unwords ["bad textMeter", show m]

-- | Is this tone acceptable to use for this pinax in this syntagma, for this
-- line number ("stropha")?
isToneLegalInPinax :: PinaxToneList -- ^ list of appropriate tones per pinax
                    -> Style        -- ^ corresponding to syntagma
                    -> PinaxLabel   -- ^ pinax enum within syntagma
                    -> Int          -- ^ 0-indexed line number (Kircher's "stropha")
                    -> Tone         -- ^ tone enum to check
                    -> Bool
isToneLegalInPinax pinaxTones style pinax lineNum tone = 
    tone /= ToneUnset && tone `elem` tones
    where
        tones | isNothing findTone = error "could not find tone in list of pinakes"
              | otherwise = fromJust findTone
              where findTone = toneset !!? (mod lineNum $ length toneset)
        toneset = tonesPerStyle style pinax pinaxTones

-- | In prose, determine 'TextMeter' based on penultimate syllable length
proseMeter :: PenultLength -> TextMeter
proseMeter l = case l of
    Long  -> ProseLong
    Short -> ProseShort

-- | All the ark settings in one structure: We use this to pass configuration
-- settings through many functions down to the core level of pulling data from
-- the ark.
data ArkConfig = ArkConfig {
    arkStyle :: Style,
    arkTone  :: Tone,
    arkToneB :: Tone, -- ^ optional second tone (only used in syntagma 2, pinax 4)
    arkMusicMeter :: MusicMeter,
    arkTextMeter  :: TextMeter
} deriving (Eq, Ord)

instance Show ArkConfig where
    show config = 
        "style: "   ++ (show $ arkStyle config) ++ 
        ", meter: " ++ (show $ arkMusicMeter config) ++ 
        ", tone: "  ++ (show $ (fromEnum $ arkTone config) + 1) ++ " "

-- ** Elements of the ark

-- *** @Vperm@: Pitch combinations for four-voice choir

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
-- part of Kircher's "rods". We need to know the vector length because it
-- varies in different /pinakes/.
data VpermTable = VpermTable {
    vpermMax :: Int,                -- ^ length of 'vperms'
    vperms   :: Vector (VpermChoir)
}

-- *** @Rperm@: Rhythm permutations to match the 'Vperm'

-- | The bottom part of the "rods" contain tables of rhythmic values written
-- with musical notes. In the simple note-against-note style, there is one
-- list of values to match each table of voices.
--
-- We implement this using our 'Dur' data type for the rhythmic values.
-- An 'Rperm' is a list of 'Dur' values.
type Rperm      = [Dur]

-- | In Syntagma I, there is only one set of rhythmic permutation that we
-- apply to all four voices of the 'VpermChoir'. But in Syntagma II, there are
-- groups of four 'Rperm's that match up with the four voices. 
-- So we make a "choir" as a vector of 'Rperm's, though in Syntagma I this
-- will always just have a single member.
type RpermChoir = Vector (Rperm)

-- | An 'RpermMeter' includes a vector of 'RpermChoir's all in one meter (see
-- the 'MusicMeter' data type above) and the length of that vector.
--
-- Kircher has a variable number of 'Rperm's in the different meters, in each
-- column, so we need to know how many there are.
--
-- In Syntagma II everything is duple meter so there is just the one meter.
data RpermMeter = RpermMeter {
    rpermMax :: Int,            -- ^ length of 'rperms'
    rperms :: Vector (RpermChoir)
}

-- | The 'RpermTable' is a vector containing all the rhythmic permutations for
-- one of Kircher's "rods".
type RpermTable = Vector (RpermMeter)

-- ** Assembling the data into Kircher's structures

-- | The ark is a box containing rods (/pinakes/), each of which includes
-- columns with voice and rhythm permutations. The rods are grouped according
-- to style into /syntagmata/, where /syntagma/ 1 is simple homorhythmic
-- counterpoint.
--
-- We implement the 'Column' as a structure with one 'VpermTable' and one
-- 'RpermTable'. 
data Column     = Column {
    colVpermTable :: VpermTable, 
    colRpermTable :: RpermTable
}

-- | A vector of 'Column' instances is a 'Pinax'.
type Pinax      = Vector (Column)

-- | A vector of 'Pinax' instances is a 'Syntagma'.
type Syntagma   = Vector (Pinax)

-- | A vector of 'Syntagma' instances plus the other elements of the physical
-- device (tone table, vocal ranges, information matching tones to pinakes)
-- makes up the full 'Arca'.
data Arca = Arca {
    perms      :: Vector (Syntagma),
    tones      :: ToneList,
    systems    :: ToneSystem,
    pinaxTones :: PinaxToneList,
    ranges     :: VoiceRanges
}

-- * Accessing the Data
-- ** By index

-- | Just get a vector value by index, safely (combining 'fromJust' and '!?')
getVectorItem :: String   -- ^ name of calling function, for debugging
              -> Vector a -- ^ vector to pull from
              -> Int      -- ^ index to select
              -> a
getVectorItem fnName vector index = maybe errorMsg id (vector !? index)
    where errorMsg = error $ unwords ["bad vector index in calling function", 
                                        fnName, show index, show $ length vector]

-- | Getting a 'Column' requires indexing through nested vectors.
-- But because there are two parts of pinax 3 in syntagma 1, we can't just use
-- the pinax label as an enum; we have to look up the number with
-- 'arca2pinax'.
column :: Arca        -- ^ ark (there's only one, but someone could make more)
        -> Style      -- ^ style label for syntagma
        -> PinaxLabel -- ^ pinax label
        -> Int        -- ^ column number
        -> Column
column arca style pinaxLabel col = thisColumn 
    where
        thisColumn   = getVectorItem "column:column" thisPinax col
        thisPinax    = arca2pinax arca style pinaxLabel

-- | Getting a 'VpermChoir' means taking the first of the 'Column' 2-tuple; we
-- select which one using a random number (from @Fortuna@ module), though the
-- Inquisition forbids chance operations
vperm :: Column 
        -> Int          -- ^ Index of voice permutation within the column
        -> VpermChoir
vperm col i = getVectorItem "vperm" (vperms vpermTable) n
    where 
        n = i `mod` vpermMax vpermTable
        vpermTable = colVpermTable col

-- __TODO__: Adjust for new structure with added layer of RpermChoir

-- | Getting an 'RpermChoir' means taking data from 'Column', using the meter
-- and a random index (for Kircher, user's choice)
rperm :: Column
        -> MusicMeter
        -> Int          -- ^ Index of rhythm permutation
        -> RpermChoir
rperm col meter i = getVectorItem "rperm" (rperms rpermTable) n
    where
        n = i `mod` rpermMax rpermTable
        rpermTable = getVectorItem "rperm:rpermTable" (colRpermTable col) $ fromEnum meter

-- ** By meaningful data

-- | The user of Kircher's arca needs only to know the number of syllables in
-- a phrase and whether the penultimate syllable is long or short. Then they
-- must freely (?) choose which table in the column.
--
-- We go straight to a voice and a rhythm permutation, given all the needed
-- variables and an index.
-- Instead of choosing freely we tempt fate and use a random number.
getVperm :: Arca 
            -> ArkConfig    -- ^ we need 'Style'
            -> Int          -- ^ syllable count
            -> Int          -- ^ line count
            -> Int          -- ^ (random) index
            -> VpermChoir
getVperm arca config sylCount lineCount i 
    | isToneLegalInPinax toneList style pinax lineCount tone = vperm col i
    | otherwise = error toneErrorMsg
    where
        toneList      = pinaxTones arca
        style         = arkStyle config
        pinax         = meter2pinax style textMeter
        tone          = toneOrToneB config lineCount
        
        col           = column arca style pinax thisColIndex
        thisColIndex  = columnIndex style textMeter sylCount lineCount
        textMeter     = arkTextMeter config

        toneErrorMsg  = unwords ["Illegal tone", show tone, 
                                 "in syntagma", show style,
                                 "pinax", show pinax,
                                 "line number", show lineCount]

-- | Use @toneB@ attribute if needed, otherwise @tone@ (We only use @toneB@
-- for florid pinax 4, every third and fourth line!)
toneOrToneB :: ArkConfig
            -> Int -- ^ line number, zero indexed
            -> Tone
toneOrToneB config lineCount 
    | style == Florid 
      && meter2pinax style meter == Pinax4 
      && lineCount `mod` 4 > 1 
        = arkToneB config 
    | otherwise 
         = arkTone config
    where
        style = arkStyle config
        meter = arkTextMeter config


-- | Select the rhythm values for a single phrase from the ark's rhythm
-- permutations (Rperms).
--
-- In Pinax 9, there is no TripleMinor category of rperms, so we screen that
-- out first. 
--
-- __TODO__: Using an error, but we could just substitute TripleMajor with a
-- note in the log (if we had a log).
getRperm :: Arca 
            -> ArkConfig    -- ^ we need 'Style' and 'MusicMeter' 
            -> Int          -- ^ syllable count
            -> Int          -- ^ line count
            -> Int          -- ^ (random) index
            -> RpermChoir
getRperm arca config sylCount lineCount i 
    | pinax == Pinax9 && arkMusicMeter config == TripleMinor
        = error "Only Duple and TripleMajor musicMeter allowed with this textMeter"
    | arkStyle config == Florid && arkMusicMeter config /= Duple
        = error "Only Duple meter allowed in Syntagma 2 for florid counterpoint"
    | otherwise 
        = rperm col meter i 
    where
        style        = arkStyle config
        col          = column arca style pinax thisColIndex
        pinax        = meter2pinax style textMeter
        thisColIndex = columnIndex style textMeter sylCount lineCount
        textMeter    = arkTextMeter config
        meter        = arkMusicMeter config


-- | The rule for selecting the column index varies depending on the /pinax/.
-- Pinax 1 and 2 are determined by whether the penultimate syllables is long
-- or short, respectively, and then the column is based on the number of
-- syllables in the phrase.
--
-- For the other /pinaces/ we are supposed to choose successive columns for
-- each "stropha" (verse line), so here we select based on the position within
-- a quatrain.
--
-- (TODO Kircher doesn't provide clear guidance about how to deal with poetry
-- that cannot or should not be grouped in quatrains, and neither do we.)
-- 
-- There are different rules for each syntagma, hence the need for Style
-- input.
columnIndex :: Style
                -> TextMeter 
                -> Int -- ^ syllable count
                -> Int -- ^ line count
                -> Int 
columnIndex style meter sylCount lineCount = 
    case style of 
        Simple -> columnIndexSimple meter 
        Florid -> columnIndexFlorid meter
    where
        proseSylCount    = sylCount - 2
        quatrainPosition = lineCount `mod` 4
        errorMsg         = "Unrecognized meter " ++ show meter 
                            ++", could not select pinax"

        columnIndexSimple meter 
            | meter == Prose    
                = error "Prose subtype not set"
            | meter `elem` [ProseLong, ProseShort]  
                = proseSylCount
            | meter `elem` [ Adonium
                           , Dactylicum
                           , IambicumEuripidaeum
                           , Anacreonticum
                           , IambicumArchilochicum
                           , IambicumEnneasyllabicum
                           , Enneasyllabicum
                           , Decasyllabicum
                           , PhaleuciumHendecasyllabicum
                           , Hendecasyllabicum
                           , Sapphicum
                           , Dodecasyllabicum
                           ]
                = quatrainPosition
            | otherwise 
                = error errorMsg

        columnIndexFlorid meter
            | meter `elem` [ Adonium
                           , Dactylicum
                           , IambicumEuripidaeum
                           , Anacreonticum
                           , IambicumArchilochicum
                           , IambicumEnneasyllabicum
                           , Enneasyllabicum
                           , Decasyllabicum
                           ]
                = quatrainPosition
            | meter `elem` [ PhaleuciumHendecasyllabicum
                           , Hendecasyllabicum
                           , Sapphicum
                           ]
                = lineCount `mod` 3 -- only three "strophes" for Sapphic (Pinax 6)
            | otherwise 
                = error errorMsg


-- | Select the pitch numbers for a single voice from one of the ark's pitch
-- permutations ('Vperm's).
getVoice :: Arca 
            -> ArkConfig    -- ^ we pass this along to 'getVperm'
            -> Int          -- ^ syllable count
            -> Int          -- ^ line count
            -> VoiceName 
            -> Int          -- ^ (random) index
            -> Vperm
getVoice arca config sylCount lineCount voice i = thisVoice
    where 
        thisVoice = getVectorItem "getVoice:voice" thisVperm $ fromEnum voice
        thisVperm = getVperm arca config sylCount lineCount i 

-- * Building the Ark

-- ** Data structures for input to build the ark

-- | Voice permutation data: 1-indexed pitch numbers, sets of four voices
-- each, usually ten sets per column
type VpermTableInput = [[Vperm]]

-- | Rhythm permutation data: 'Dur' values, three sets for different meters,
-- each containing either one set per voice permutation set (/syntagma I/) or
-- a four-voice set to match (/syntagma II/)
type RpermTableInput = [[[Rperm]]]

-- | Column data: Pairs of input data for voice and rhythm permutations
type ColumnInput     = (VpermTableInput, RpermTableInput)

-- | Pinax data: List of data for columns
type PinaxInput      = [ColumnInput]

-- ** Transforming input data to ark structures

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

-- | Make a new 'VpermTable' that knows its own length: Application of
-- 'fromList2D' to 'Vperm'
buildVpermTable :: VpermTableInput -> VpermTable
buildVpermTable ls = VpermTable {
    vpermMax = length ls,
    vperms   = fromList2D ls
}

-- | Make a new 'RpermMeter' that knows its own length.
newRpermMeter :: [[Rperm]] -> RpermMeter
newRpermMeter theseRperms = RpermMeter {
    rpermMax = length theseRperms,
    rperms   = fromList2D theseRperms
}

-- | Build an 'RpermTable' with 'RpermMeter's that know their length.
buildRpermTable :: RpermTableInput -> RpermTable
buildRpermTable ls = fromList $ map newRpermMeter ls

-- | Build a 'Column' directly from input data: two nested lists, one for all
-- the voice permutations in the column and the other for all the rhythm
-- permutations.
-- Because we are manually entering Kircher's data for the ark we do not check
-- for validity here, and there are several variations across the /syntagmata/
-- and /pinakes/ in how the data is structured.
buildColumn :: ColumnInput -> Column
buildColumn (vperms, rperms) = Column (buildVpermTable vperms) (buildRpermTable rperms)

-- | Build a 'Pinax' from pairs of 'VpermTable' and 'RpermTable' data
buildPinax :: PinaxInput -> Pinax
buildPinax = fromList . (map buildColumn)

-- | Build a 'Syntagma' from constructed 'Pinax' items (not from raw input)
buildSyntagma :: [Pinax] -> Syntagma
buildSyntagma = fromList

-- * Pull out values simply for testing

-- | Pull out a single 'Column' given indices
columnFromArca :: Arca 
                -> Int -- ^ syntagma index
                -> Int -- ^ pinax index
                -> Int -- ^ column index
                -> Column
columnFromArca arca syntagmaNum pinaxNum columnNum = thisColumn
    where
        thisColumn   = getVectorItem "columnFromArca" thisPinax columnNum
        thisPinax    = getVectorItem "columnFromArca:pinax" thisSyntagma pinaxNum
        thisSyntagma = getVectorItem "columnFromArca:syntagma" (perms arca) syntagmaNum

-- | Pull out a single 'Vperm', which is a list of 'Int'
vpermFromArca :: Arca 
                -> Int -- ^ syntagma index
                -> Int -- ^ pinax index
                -> Int -- ^ column index
                -> Int -- ^ vperm (row) index
                -> Int -- ^ voice (SATB) index
                -> Vperm
vpermFromArca arca syntagmaNum pinaxNum columnNum vpermNum voiceNum = thisVoice
    where
        thisVoice  = getVectorItem "vpermFromArca:voice" thisVperm voiceNum
        thisVperm  = getVectorItem "vpermFromArca:vperm" vpermTable vpermNum 
        vpermTable = vperms $ colVpermTable thisColumn
        thisColumn = columnFromArca arca syntagmaNum pinaxNum columnNum

-- | Pull out a single 'Rperm', which is a list of 'Dur'
rpermFromArca :: Arca
                -> Int -- ^ syntagma index
                -> Int -- ^ pinax index
                -> Int -- ^ column index
                -> Int -- ^ meter index
                -> Int -- ^ rperm index
                -> Int -- ^ voice index
                -> Rperm
rpermFromArca arca syntagmaNum pinaxNum columnNum meterNum rpermNum voiceNum = thisRperm
    where
        thisRperm       = getVectorItem "rpermFromArca:rperm" thisRpermMeter voiceNum
        thisRpermMeter  = getVectorItem "rpermFromArca:rpermChoir" thisRpermChoir rpermNum
        thisRpermChoir  = rperms 
            $ getVectorItem "rpermFromArca:rpermTable" (colRpermTable thisColumn) meterNum
        thisColumn      = columnFromArca arca syntagmaNum pinaxNum columnNum


