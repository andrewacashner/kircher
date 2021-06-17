{-|
Module      : Aedifico
Description : Data structures for building Kircher's /Arca musarithmica/
Copyright   : (c) Andrew A. Cashner 2020
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
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
    ( 
        Maybe,
        fromJust
    )

import Data.Vector 
    (
        Vector, 
        (!),
        (!?),
        fromList
    )

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
    | Decasyllabicum                -- ^ 10 syllables, penultimate short
    | PhaleuciumHendecasyllabicum   -- ^ 11 syllables
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
    "Decasyllabicum"              -> Decasyllabicum
    "PhaleuciumHendecasyllabicum" -> PhaleuciumHendecasyllabicum 
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
    Decasyllabicum              -> 10
    PhaleuciumHendecasyllabicum -> 11 
    Sapphicum                   -> 11
    Dodecasyllabicum            -> 12
    _ -> error "bad meter"


-- *** Style

-- | Kircher has a number of styles but we are so far only using simple
-- (note-against-note homorhythmic polyphony).
--
-- __TODO__ implement other styles.
data Style = Simple | Florid
    deriving (Enum, Eq, Ord)

instance Show Style where
    show style = case style of 
        Simple -> "Simple"
        Florid -> "Florid"

-- | Select style by string
toStyle :: String -> Style
toStyle s = case s of
    "Simple"    -> Simple
    "Florid"    -> Florid
    _           -> error $ unwords ["Unknown style", s]

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
    _ -> error $ unwords ["Unknown mode", s]

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

-- | 'Pinax' maps to 'TextMeter'
data PinaxLabel =  
      Pinax1 
    | Pinax2
    | Pinax3
    | Pinax4
    | Pinax5
    | Pinax6
    | Pinax7
    | Pinax8
    | Pinax9
    | Pinax10
    | Pinax11
    | PinaxNil
    deriving (Show, Enum, Ord, Eq)

-- | Get pinax from textual meter
meter2pinax :: Style -> TextMeter -> PinaxLabel
meter2pinax s m = case s of
        Simple -> meter2pinaxSimple m
        Florid -> meter2pinaxFlorid m

        where
            meter2pinaxSimple m = case m of
                Prose       -> error "Need to determine ProseShort or ProseLong"
                ProseLong                   -> Pinax1
                ProseShort                  -> Pinax2
                Adonium                     -> Pinax3
                Dactylicum                  -> Pinax3
                IambicumEuripidaeum         -> Pinax4
                Anacreonticum               -> Pinax5
                IambicumArchilochicum       -> Pinax6
                IambicumEnneasyllabicum     -> Pinax7
                Decasyllabicum              -> Pinax8
                PhaleuciumHendecasyllabicum -> Pinax9
                Sapphicum                   -> Pinax10
                Dodecasyllabicum            -> Pinax11
                _ -> error $ unwords ["bad textMeter", show m]

            meter2pinaxFlorid m = case m of
                Adonium -> Pinax1
                _ -> error $ unwords ["bad textMeter", show m]


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
    arkMode  :: Mode,
    arkMusicMeter :: MusicMeter,
    arkTextMeter  :: TextMeter
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
-- part of Kircher's "rods". We need to know the vector length because it
-- varies in different /pinakes/.
data VpermTable = VpermTable {
    vpermMax :: Int,                -- ^ length of 'vperms'
    vperms   :: Vector (VpermChoir)
}

-- *** 'Rperm': Rhythm permutations to match the 'Vperm'

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
column :: Arca        -- ^ ark (there's only one, but someone could make more)
        -> Int        -- ^ syntagma number
        -> PinaxLabel -- ^ pinax label enum 
        -> Int        -- ^ column number
        -> Maybe Column
column arca syntagma pinax col = thisPinax !? col
    where
        thisPinax    = fromJust $ thisSyntagma !? (fromEnum pinax)
        thisSyntagma = fromJust $ (perms arca) !? syntagma

-- | Getting a 'VpermChoir' means taking the first of the 'Column' 2-tuple; we
-- select which one using a random number (from @Fortuna@ module), though the
-- Inquisition forbids chance operations
vperm :: Column 
        -> Int          -- ^ Index of voice permutation within the column
        -> VpermChoir
vperm col i = (vperms vpermTable) ! n
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
rperm col meter i = (rperms rpermTable) ! n
    where
        n = i `mod` rpermMax rpermTable
        rpermTable = (colRpermTable col) ! fromEnum meter

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
            -> Int          -- ^ syllable count
            -> Int          -- ^ line count
            -> Int          -- ^ (random) index
            -> VpermChoir
getVperm arca config sylCount lineCount i = vperm col i
    where

        style         = arkStyle config
        styleNum      = fromEnum style
        col           = checkColumn "getVperm" $ column arca styleNum pinax thisColIndex
        pinax         = meter2pinax style textMeter
        thisColIndex  = columnIndex style textMeter sylCount lineCount
        textMeter     = arkTextMeter config

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
    | otherwise 
        = rperm col meter i 
    where
        style        = arkStyle config
        styleNum     = fromEnum style
        col          = checkColumn "getRperm" $ column arca styleNum pinax thisColIndex
        pinax        = meter2pinax style textMeter
        thisColIndex = columnIndex style textMeter sylCount lineCount
        textMeter    = arkTextMeter config
        meter        = arkMusicMeter config

-- | Get a 'Column' out of a 'Maybe Column', or an error if it was 'Nothing'
checkColumn :: String -- ^ name of the function calling this one
            -> Maybe Column 
            -> Column
checkColumn functionName col = case col of
    Nothing  -> error $ "Could not find column for " ++ functionName
    Just col -> col


-- | The rule for selecting the column index varies depending on the /pinax/.
-- Pinax 1 and 2 are determined by whether the penultimate syllables is long
-- or short, respectively, and then the column is based on the number of
-- syllables in the phrase.
-- Pinax 3 columns are based on whether the meter is Adonic or Dactylic (5 or
-- six syllables respectively).
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
        errorMsg         = "Unrecognized meter, could not select pinax"

        columnIndexSimple meter 
            | meter == Prose    
                = error "Prose subtype not set"
            | meter `elem` [ProseLong, ProseShort]  
                = proseSylCount
            | meter == Adonium
                = 0
            | meter == Dactylicum
                = 1
            | meter `elem` [IambicumEuripidaeum,
                            Anacreonticum, 
                            IambicumArchilochicum,
                            IambicumEnneasyllabicum,
                            Decasyllabicum, 
                            PhaleuciumHendecasyllabicum,
                            Sapphicum,
                            Dodecasyllabicum]
                = quatrainPosition
            | otherwise 
                = error errorMsg

        columnIndexFlorid meter
            | meter == Adonium 
                = 0
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
getVoice arca config sylCount lineCount voice i = 
    getVperm arca config sylCount lineCount i ! fromEnum voice


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

-- | Make a new 'VpermTable' that knows its own length: Application of
-- 'fromList2D' to 'Vperm'
buildVpermTable :: [[Vperm]] -> VpermTable
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
buildRpermTable :: [[[Rperm]]] -> RpermTable
buildRpermTable ls = fromList $ map newRpermMeter ls


-- | Build a Pinax (a vector from a list of 'Column's)
buildPinax :: [Column] -> Pinax
buildPinax = fromList 


-- * Pull out values simply for testing

-- | Pull out a single Vperm :: [Int] 
vpermFromArca :: Arca 
                    -> Int -- ^ syntagma index
                    -> Int -- ^ pinax index
                    -> Int -- ^ column index
                    -> Int -- ^ vperm (row) index
                    -> Int -- ^ voice (SATB) index
                    -> Vperm
vpermFromArca arca syntagmaNum pinaxNum columnNum vpermNum voiceNum = 
    vpermTable ! vpermNum ! voiceNum
    where
        vpermTable = vperms $ colVpermTable column
        column = perms arca ! syntagmaNum ! pinaxNum ! columnNum

-- | Pull out a single Rperm :: [Dur]
rpermFromArca :: Arca
                    -> Int -- ^ syntagma index
                    -> Int -- ^ pinax index
                    -> Int -- ^ column index
                    -> Int -- ^ meter index
                    -> Int -- ^ rperm index
                    -> Int -- ^ voice index
                    -> Rperm
rpermFromArca arca syntagmaNum pinaxNum columnNum meterNum rpermNum voiceNum = 
    rpermChoir ! rpermNum ! voiceNum
    where
        rpermChoir  = rperms $ colRpermTable column ! meterNum
        column      = perms arca ! syntagmaNum ! pinaxNum ! columnNum


