{-|
Module      : Cogito.Musarithmetic
Description : Data structures and functions to adjust and store the ark's music output
Copyright   : (c) Andrew A. Cashner 2021
Maintainer  : Andrew Cashner, <andrew.cashner@rochester.edu>
Stability   : Experimental

This module provides the tools used in the main @Cogito@ module to adjust
music created by the ark and to store it in internal structures that will then
be used by the @Scribo@ modules.

The `stepwiseVoiceInRange` function tests all the possible permutations of
octaves for the pitches in a phrase and finds the best path, with the minimum
of large leaps and notes out of range.

Kircher's specification for how to put voices in range is incomplete, and his
own implementation is inconsistent, as demonstrated by his examples.  He says
to find the next closest pitch "within the octave" among the notes on the
staff (i.e., the notes within range), but he doesn't define "within the
octave." Sometimes he leaps a fifth instead of a fourth, which would break
that rule.

Sometimes a gesture requires the voice to go out of range. Kircher says in
that case you can switch clefs. But that doesn't change the notes a singer can
sing. If he means to change to transposing clefs, that might work, but no one
ever changed to transposing clefs for only a single phrase and then went back. 

Instead, this module provides an algorithm that works every time to produce an
optimal melody with a small ambitus, minimum number of notes outside of range,
and small leaps. This seems very close to what Kircher probably thought
musicians would do intuitively, but did not fully specify programmatically.
-}

module Cogito.Musarithmetic where

import Data.List 
    (minimumBy)

import Data.Maybe
    ( fromJust
    , isNothing
    , maybe
    )

import Data.Function
    (on)

import Aedifico 
    ( Accid        (..)
    , AccidType    (..)
    , Arca         (..)
    , ArkConfig    (..)
    , Dur          (..)
    , Tone         (..)
    , ToneList
    , ToneSystem
    , Octave       (OctNil)
    , Pnum         (..)
    , System       (..)
    , VoiceName    (..)
    , VoiceRange   (..)
    , VoiceRanges  (..)
    , TextMeter    (..)
    , Pitch        (..)
    , PnumAccid
    , getRange
    , getVectorItem
    , simplePitch
    , toneOrToneB
    )

-- * Data structures

-- ** For storing and adjusting pitches and rhythms, not including the sung text

-- | A 'Voice' is a list of pitches with an identifier for the voice type.
data Voice = Voice {
    voiceID :: VoiceName, -- ^ Enum for Soprano, Alto, Tenor or Bass
    music   :: [Pitch]    
} deriving (Show, Eq, Ord)

-- | A 'Chorus' is a group (list) of four 'Voice' items
--
-- __TODO__: We don't actually define it as being four items.
-- __TODO__: Do we still need this with new MEI setup?
type Chorus = [Voice] 

-- ** For storing music including the sung text

-- | A 'Note' contains a pitch and a syllable, equivalent to MEI @note@
data Note = Note {
    notePitch :: Pitch,
    noteSyllable :: Syllable
} deriving (Show, Eq, Ord)

-- | A 'Syllable' is a single syllable to be paired with a 'Pitch', including
-- its position in the word.
data Syllable = Syllable {
    sylText :: String,
    sylPosition :: SyllablePosition
} deriving (Show, Eq, Ord)

-- | What is the position of the syllable relative to the word? Beginning,
-- middle, or end? This determines hyphenation.
data SyllablePosition =   First 
                        | Middle
                        | Last
                        | Only
                        | Tacet -- ^ no syllable
                        deriving (Show, Enum, Eq, Ord)

-- | A 'MusicPhrase' contains all the notes set using one permutation drawn
-- from the ark, for a single voice.
data MusicPhrase = MusicPhrase {
    phraseVoiceID :: VoiceName,
    notes :: [Note]
} deriving (Show, Eq, Ord)

-- | A list of 'MusicPhrase' items
type MusicSentence  = [MusicPhrase]

-- | A 'MusicSection' contains all the music for one section in the input XML
-- document, for a single voice, together with the parameters set in the input
-- file.
data MusicSection = MusicSection {
    secVoiceID :: VoiceName,
    secConfig :: ArkConfig,
    secSentences :: [MusicSentence]
}

-- | A 'MusicChorus' is a four-voice SATB structure of 'MusicSection' data.
-- __TODO__ do we really need it to be structured this way?
data MusicChorus = MusicChorus {
    soprano :: MusicSection,
    alto    :: MusicSection,
    tenor   :: MusicSection,
    bass    :: MusicSection
}

-- | The full 'MusicScore' is a list of SATB 'MusicChorus' structures.
type MusicScore     = [MusicChorus]


-- * Manipulating the @Pitch@

-- | Create a rest (that is, a 'Pitch' with duration only)
--
-- We make a 'Pitch' but set the 'pnum' to 'Rest'; 'oct' and 'accid' are set
-- to special nil values ('OctNil', 'AccidNil')
--
-- __TODO__: We are setting the octave using @fromEnum OctNil@: Isn't this the
-- same as just setting it to zero? Is there a better way to mark this?
newRest :: Dur      -- ^ Rhythmic duration for this note
        -> Pitch    
newRest d = Pitch {
    pnum = Rest,
    dur = d,
    oct = fromEnum OctNil,
    accid = AccidNil,
    accidType = None
}

-- ** Adjust pitch for tone

-- | Is a tone in /cantus mollis/? Should there be a flat in the key
-- signature?
toneMollis :: Tone -> ToneSystem  -> Bool
toneMollis tone systems =
    let s = getVectorItem "toneMollis:systems" systems $ fromEnum tone
    in case s of
        Durus  -> False
        Mollis -> True

-- | Adjust a pitch to be in a given tone. 
pnumAccidInTone :: Int -> ToneList -> Tone -> PnumAccid
pnumAccidInTone rawPnum toneList tone = pnum
    where 
        pnum        = getVectorItem "pnumAccidInTone:pnum" toneScale rawPnum
        toneScale   = getVectorItem "pnumAccidInTone:toneScale" toneList $ fromEnum tone
  
-- | Get the modal final for this tone. What pitch = 0 in this tone? (In
-- Kircher's 1-indexed vperms, the final is 1 or 8.)
modalFinal :: ToneList -> Tone -> Pitch
modalFinal toneList tone = simplePitch (pnum, 0)
    where 
        pnum      = fst finalPair
        finalPair = getVectorItem "modalFinalInRange:finalPair" toneScale 0
        toneScale = getVectorItem "modalFinalInRange:toneScale" toneList $ fromEnum tone


-- ** Check for rests

-- | Check to see if a rhythmic duration is a rest type (the rest enums begin
-- with 'LgR' so we compare with that)
isRest :: Dur -> Bool
isRest dur = dur >= LgR 

-- | Is the 'Pitch' a rest?
isPitchRest :: Pitch -> Bool
isPitchRest p = pnum p == Rest

-- | Are any of these pitches rests?
anyRests :: [Pitch] -> Bool
anyRests = any isPitchRest 


-- * Measure distances between notes and correct bad intervals

-- ** Convert between diatonic and chromatic pitches for calculations

-- | Convert 'Pitch' to absolute pitch number, using chromatic calculations
-- (base 12). Raise an error if it is a rest.
absPitch :: Pitch -> Int
absPitch p 
    | isPitchRest p = error "Can\'t convert Rest to absolute pitch" 
    | otherwise     = oct12 + pnum12 + accid12
    where
        oct12   = oct p * 12
        pnum12  = dia2chrom $ pnum p
        accid12 = (fromEnum $ accid p) - 2


-- | Get chromatic offset from C for diatonic pitch classes 
-- (@PCc -> 0@, @PCd -> 2@, @PCe -> 4@, etc.)
dia2chrom :: Pnum -> Int
dia2chrom n = case n of
    PCc  -> 0
    PCd  -> 2
    PCe  -> 4
    PCf  -> 5
    PCg  -> 7
    PCa  -> 9
    PCb  -> 11
    PCc8 -> 12
    _    -> error $ "Unknown pitch class" ++ show n

-- | Absolute diatonic pitch (base 7). Raise an error if it is a rest.
absPitch7 :: Pitch -> Int
absPitch7 p | isPitchRest p = error "can't take absPitch7 of a rest"
            | otherwise     = oct p * 7 + (fromEnum $ pnum p)

-- ** \"Musarithmetic\": Differences, sums, conditionals with @Pitch@ values

-- | Do mathematical operations on pitches (using their chromatic 'absPitch' values)
pitchMath :: (Int -> Int -> Int) -> Pitch -> Pitch -> Int
pitchMath f = f `on` absPitch

-- | Do mathematical operations on pitches (using their diatonic 'absPitch7' values)
pitchMath7 :: (Int -> Int -> Int) -> Pitch -> Pitch -> Int
pitchMath7 f = f `on` absPitch7

-- | Do boolean tests on pitches (using their 'absPitch' values)
pitchTest :: (Int -> Int -> Bool) -> Pitch -> Pitch -> Bool
pitchTest f = f `on` absPitch

-- ** Conditional tests

-- | Are two 'Pitch'es the same chromatic pitch, enharmonically equivalent?
pEq :: Pitch -> Pitch -> Bool
pEq = pitchTest (==)

-- | Test the pitch label and accidental of a 'Pitch'
pnumAccidEq :: Pnum -> Accid -> Pitch -> Bool
pnumAccidEq thisPnum thisAccid p = pnum p == thisPnum && accid p == thisAccid

-- | Pitch greater than?
pGt :: Pitch -> Pitch -> Bool
pGt = pitchTest (>)

-- | Pitch less than?
pLt :: Pitch -> Pitch -> Bool
pLt = pitchTest (<)

-- | Pitch greater than or equal?
pGtEq = pitchTest (>=)

-- | Pitch less than or equal?
pLtEq = pitchTest (<=)

-- ** Differences, intervals

-- | Difference between pitches, chromatic interval
p12diff :: Pitch -> Pitch -> Int
p12diff p1 p2 | anyRests [p1, p2] = 0
              | otherwise         = pitchMath (-) p1 p2

-- | Chromatic difference between pitch classes (within one octave); 'p12diff' modulo 12
p12diffMod :: Pitch -> Pitch -> Int
p12diffMod p1 p2 = p12diff p1 p2 `mod` 12

-- | Difference between pitches, diatonic interval
-- Unison = 0, therefore results of this function are one less than the verbal
-- names of intervals (@p7diff = 4@ means a fifth)
p7diff :: Pitch -> Pitch -> Int
p7diff p1 p2 | anyRests [p1, p2] = 0
             | otherwise         = pitchMath7 (-) p1 p2

-- | Diatonic difference between pitch classes (= pitch difference as though
-- within a single octave); result is 0-indexed, so the interval of a "third"
-- in speech has a @p7diffMod@ of 2
p7diffMod :: Pitch -> Pitch -> Int
p7diffMod p1 p2 = p7diff p1 p2 `mod` 7

-- | Take the absolute value of an intervals, the difference between pitches.
-- The interval between any note and a rest is zero.
absInterval :: Pitch -> Pitch -> Int
absInterval p1 p2 | anyRests [p1, p2] = 0
                  | otherwise         = abs $ p7diff p1 p2

-- ** Change a @Pitch@ based on calculation

-- | Change the pitch class and octave of an existing 'Pitch' to that of an
-- absolute diatonic pitch number. Return rests unchanged.
changePnumOctave :: Int -> Pitch -> Pitch
changePnumOctave n p 
    | isPitchRest p = p 
    | otherwise     = Pitch {
        pnum      = toEnum $ n `mod` 7,
        oct       = n `div` 7,
        dur       = dur p,
        accid     = accid p,
        accidType = accidType p
    }

-- *** Operate on pitch class and octave

-- | Increase a pitch diatonically by a given interval (0-indexed diatonic,
-- e.g., @p7inc p 4@ raises @p@ by a diatonic third). Return rests unchanged.
p7inc :: Pitch 
      -> Int -- ^ diatonic interval, 0-indexed
      -> Pitch
p7inc p n | isPitchRest p = p
          | otherwise     = changePnumOctave (n + absPitch7 p) p

-- *** Operate on octave alone

-- | Just change the octave to a given number, no calculation required
octaveChange :: Pitch 
             -> Int -- ^ Helmholtz octave number
             -> Pitch
octaveChange p n = Pitch { 
    pnum      = pnum p,
    oct       = n,
    dur       = dur p,
    accid     = accid p,
    accidType = accidType p
}

-- | Increase the octave number by the given amount
octaveInc :: Pitch -> Int -> Pitch
octaveInc p n = octaveChange p (oct p + n)

-- | Raise the octave by 1
octaveUp :: Pitch -> Pitch
octaveUp p = p `octaveInc` 1 

-- | Lower the octave by 1
octaveDown :: Pitch -> Pitch
octaveDown p = p `octaveInc` (-1)

-- ** Test a @Pitch@ relative to a @VoiceRange@

-- | Is the pitch below the bottom limit of the voice range?
pitchTooLow :: VoiceRange -> Pitch -> Bool
pitchTooLow range p = p `pLt` low range

-- | Is the pitch above the upper limit of the voice range?
pitchTooHigh :: VoiceRange -> Pitch -> Bool
pitchTooHigh range p = p `pGt` high range 

-- | Is the 'Pitch' within the proper range for its voice? Rests automatically
-- count as valid.
pitchInRange :: VoiceRange -> Pitch -> Bool
pitchInRange range p = isPitchRest p ||
    (not $ pitchTooLow range p || pitchTooHigh range p)

-- | Is this an acceptable leap? Only intervals up to a sixth, or an octave are
-- okay. If either note is a rest, then that also passes the test.
--
-- TODO: Ignoring rests like this is a bit of a cop-out, but Kircher usually
-- puts rests at the beginning of a phrase, so they affect the interval
-- /between/ phrases, which we are not adjusting anyway. In an ideal scenario,
-- we would.
legalLeap :: Pitch -> Pitch -> Bool
legalLeap p1 p2 | anyRests [p1, p2] = True
                | otherwise         = diff <= 7 && diff /= 6
    where diff = absInterval p1 p2


-- * Find an optimal version of the melody for a particular voice 

-- ** Make lists of pitches in range

-- | Find the lowest valid instance of a given 'Pitch' within the
-- 'VoiceRange'. This is used to calculate an optimal path through the
-- possible pitches in a phrase, and means that in most cases the melody will
-- end up in the lower end of the voice's range.
lowestInRange :: VoiceRange -> Pitch -> Pitch
lowestInRange range p 
    | pitchInRange range p = p
    | pitchTooLow    range p = lowestInRange range $ octaveUp p
    | otherwise              = lowestInRange range $ octaveChange p (oct $ low range)

-- | List all the octaves within a voice range.
octavesInRange :: VoiceRange -> [Int]
octavesInRange range = [oct $ low range .. oct $ high range]

-- | List all the valid instances of a given pitch within a voice range.
pitchesInRange :: VoiceRange -> Pitch -> [Pitch]
pitchesInRange range p = filter (pitchInRange expandedRange) candidates
    where 
        candidates    = map (octaveChange p) $ octavesInRange range
        expandedRange = VoiceRange {
            low  = low range `p7inc` (-2),
            high = high range `p7inc` 2
        }

-- | Given a list of pitches (taken from the vperms in the ark), return a list
-- of list of all the valid instances of those pitches within a particular
-- voice range. This determines the candidate pitches that we will test to
-- find the optimal melody.
pitchCandidates :: VoiceRange -> [Pitch] -> [[Pitch]]
pitchCandidates range = map (pitchesInRange range)

-- ** Decision trees for evaluation ordered permutations of a series. 

-- | Binary tree. We use a left-child/right-sibling binary tree to evaluate
-- any number of candidates for each element in the series.
data Btree a = Empty | Node a (Btree a) (Btree a)
    deriving (Show)

-- | Build a general tree, implemented as left-child/right-sibling binary tree that
-- can take more than two options at each level
tree :: [[a]] -> Btree a
tree []          = Empty
tree ((x:[]):[]) = Node x Empty Empty                -- no children or siblings
tree ((x:[]):ys) = Node x (tree ys) Empty            -- children but no siblings
tree ((x:xs):[]) = Node x Empty (tree [xs])          -- siblings but no children
tree ((x:xs):ys) = Node x (tree ys) (tree ((xs):ys)) -- both 

-- *** Test ordered permutations with a tree

-- | Build a left-child/right-sibling tree from a list of the options at each
-- level, only including options that pass a test function; the test function
-- compares each parent to its child. If the value of the parent (previous
-- good value) is 'Nothing' then we know it is the beginning of the tree,
-- there is no previous value to compare.
testTree :: (a -> a -> Bool) -- ^ test to determine if child is valid relative to parent
         -> Maybe a          -- ^ previous value to test
         -> [[a]]            -- ^ list of permutations at each level
         -> Btree a

-- End of line.
testTree f _ [] = Empty

-- No children or siblings: If x is good, make it a final node.
testTree f p ((x:[]):[]) 
    | isNothing p || f (fromJust p) x = Node x Empty Empty 
    | otherwise = Empty

-- Children but no siblings: If x is good, make a node and follow its
-- children, comparing them to x.
testTree f p ((x:[]):ys) 
    | isNothing p || f (fromJust p) x = Node x childTree Empty 
    | otherwise = Empty
    where childTree = testTree f (Just x) ys

-- Siblings but no children: If x is good, make a node and follow its
-- siblings. Compare its siblings to the parent of x.
testTree f p ((x:xs):[]) 
    | isNothing p || f (fromJust p) x = Node x Empty siblingTree 
    | otherwise = siblingTree
    where siblingTree = testTree f p [xs]

-- Both children and siblings: If x is good, make a node and follow both
-- children (compare to x) and siblings. Compare siblings to the parent of x.
testTree f p ((x:xs):ys) 
    | isNothing p || f (fromJust p) x = Node x childTree siblingTree
    | otherwise = siblingTree 
    where 
        childTree   = testTree f (Just x) ys
        siblingTree = testTree f p ((xs):ys)

-- ** Traversal

-- | Make a list of all good paths in an LCRS tree. If no good paths are
-- found, the result will be @[]@.
paths :: [[a]] -- ^ accumulator list
      -> Btree a 
      -> [[a]]
paths xs Empty = map reverse xs 
paths [] (Node n l r)               = paths [[n]] l ++ paths [] r
paths ((x:xs):ys) (Node n l Empty)  = paths ((n:x:xs):ys) l
paths ((x:xs):ys) (Node n l r)      = paths ((n:x:xs):ys) l ++ paths ((x:xs):ys) r

-- ** Test the paths
-- | Are all the elements of a list the same length?
sameLengths :: [[a]] -> Bool
sameLengths [] = True
sameLengths (x:xs) = all (== length x) $ map length xs

-- | Prune out paths that are shorter than the original list of items. If none
-- are left after pruning (no viable paths), return 'Nothing'.
fullPaths :: [a]   -- ^ list of items to permute
          -> [[b]] -- ^ list of permutations
          -> Maybe [[b]]
fullPaths items options | null paths = Nothing
                        | otherwise  = Just paths
    where paths = filter ((== length items) . length) options

-- ** Score a path for \"badness\" of different kinds

-- | The ambitus is the widest range of pitches used; the difference between
-- the highest and lowest pitches. Ignore rests.
ambitus :: [Pitch] -> Int
ambitus ps = maximum aps - minimum aps
    where aps = map absPitch7 $ filter (not . isPitchRest) ps

-- | Calculate and list intervals between pitches in a list. The list will be
-- one item shorter than the list of inputs.
intervals :: [Pitch] -> [Int]
intervals (a:[])    = []
intervals (a:b:[])  = [absInterval a b]
intervals (a:b:cs)  = (absInterval a b):(intervals (b:cs))

-- | Add up all the intervals larger than a fourth (where p7diff > 3 with
-- 0-indexed intervals).
sumBigIntervals :: [Pitch] -> Int
sumBigIntervals = sum . filter (> 3) . intervals

-- | Find all the pitches that exceed a given range, and add up the interval
-- by which they go above or below the limits.
sumBeyondRange :: VoiceRange -> [Pitch] -> Int
sumBeyondRange range ps = sum $ map sum [highDegrees, lowDegrees]
    where
        pitches     = filter (not. isPitchRest) ps
        highs       = filter (pitchTooHigh range) pitches
        lows        = filter (pitchTooLow range) pitches
        highDegrees = map (\p -> absInterval p $ high range) highs
        lowDegrees  = map (\p -> absInterval p $ low range) lows

-- | Calculate weighted "badness" score for a list of pitches. Sum of ambitus,
-- sum of large intervals (x 2), sum of degrees of notes out of range (x 10).
badness :: VoiceRange -> [Pitch] -> Int
badness range [] = error "No paths found to test!"
badness range ps = sum [ ambitus ps
                       , sumBigIntervals ps * 2
                       , sumBeyondRange range ps * 10
                       ]

-- | Find the best path (first with lowest "badness"), or raise error if none
-- found
bestPath :: VoiceRange -> [Pitch] -> [[Pitch]] -> [Pitch]
bestPath range pnames = maybe (error "No path found") 
                              (leastBadPath range) . fullPaths pnames

-- | Choose the path with the lowest "badness"; if there are multiple with the
-- same score, choose the first
leastBadPath :: VoiceRange -> [[Pitch]] -> [Pitch]
leastBadPath range = minimumBy (compare `on` (badness range))

-- ** Synthetic functions pulling together the above

-- | Build a tree of all pitch sequences with appropriate leaps
stepwiseTree :: [[Pitch]] -> Btree Pitch
stepwiseTree = testTree legalLeap Nothing

-- | Find a melody for a voice with an optimal blend of avoiding bad leaps and
-- staying within range. This is the main function used in @Cogito@.
--
-- Avoid large or illegal leaps and stay as much in range
-- as possible. For example,
-- some melodies have long stepwise ascents or descents which, in certain
-- tones, will take the voice out of range, and if we adjust them in the
-- middle, we will get an illegal seventh interval. 
--
-- We build a list of candidate pitches within the range, then we build a tree
-- of the ordered permutations of those pitches and test the paths according
-- to a subjective "badness" rating, including the ambitus or total range of
-- highest to lowest notes, the number and size of large intervals, and the
-- number of notes out of range (and how much out of range they are). The
-- first path with the lowest score wins.
stepwiseVoiceInRange :: VoiceRanges -> Voice -> Voice
stepwiseVoiceInRange ranges v = Voice {
    voiceID = voiceID v,
    music   = adjust
}
    where
        pitches     = music v
        range       = getRange (voiceID v) ranges
        candidates  = pitchCandidates range pitches
        options     = stepwiseTree candidates
        adjust      = bestPath range pitches $ paths [] options


