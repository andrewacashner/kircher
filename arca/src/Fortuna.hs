{-|
Module      : Fortuna
Description : (Theologically) dangerous chance operations: generate random indices
Copyright   : (c) 2022 Andrew A. Cashner
Stability   : Experimental

Kircher's ark does require a small amount of free choice on the part of the
user, in selecting which voice permutation and rhythm permutation to use from
each column.  We take the (for Kircher) theologically dangerous route of
chance operations and generate a random number for the index of the voice and
rhythm permutations.
-}

module Fortuna where

import System.Random 
    ( getStdRandom
    , randomR
    )

import Control.Monad 
    (replicateM)

import Lectio
    ( PhrasesInLyricSentence
    , PhrasesInLyricSection
    )

-- | A @Perm@ stores the random number choices used to select voice and rhythm
-- permutations.
data Perm = Perm {
    voiceIndex  :: Int,
    rhythmIndex :: Int
} deriving (Read)

instance Show Perm where
    show perm = "v" ++ vperm ++ "/r" ++ rperm 
        where
            vperm = show $ voiceIndex perm
            rperm = show $ rhythmIndex perm

-- | Make a pair of random numbers to select vperm and rperm:
--
--   - vperm is always one of 10 vperms per column
--   - rperm lists are variable in length, so we choose a larger number and
--   then take the modulo of the length of the list once it's selected (in @Aedifico@)
choosePerms :: IO Perm
choosePerms = do
    let 
        vpermBounds = (0, 99)
        rpermBounds = (0, 99)
    v <- getStdRandom (randomR vpermBounds)
    r <- getStdRandom (randomR rpermBounds)

    let p = Perm { voiceIndex = v, rhythmIndex = r }
    return p

-- | Each sentence needs a list of perms, one per phrase
type SentencePerm   = [Perm]

-- | A list of perms for each sentence in the section
type SectionPerm    = [SentencePerm]

-- | Generate a list of 'Perm's of a given length to match a @Sentence@
sentencePerms :: PhrasesInLyricSentence -- ^ number of permutations
        -> IO SentencePerm
sentencePerms n = replicateM n choosePerms

-- | Generate perms for a whole section
sectionPerms :: PhrasesInLyricSection -> IO SectionPerm
sectionPerms ps = mapM (\ p -> sentencePerms p) ps

-- | Generate perms for the whole input structure
inputPerms :: [PhrasesInLyricSection] -> IO [[SentencePerm]]
inputPerms ss = mapM (\ s -> sectionPerms s) ss




