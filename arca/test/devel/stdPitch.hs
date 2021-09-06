-- older method of setting and adjusting pitches (quasi object-oriented)
-- retired 2021/09/06

-- | Standardize pitch.
--
-- A 'Pitch' is like a two-digit number in base 7, where the first digit (the
-- "7s") is the octave. If the second digit (the "1s") is over 7, we must
-- increment the octave. A "standardized pitch" in our implementation is one
-- that conforms to this requirement, so the 'stdPitch' method takes all the
-- input for a 'Pitch' and does the necessary conversions to set the octave.
--
-- We need this because Kircher's tables include both pitch number 1 and pitch
-- 8. If pitch input is Kircher's pitch 8, then set pitch num to PcC (0) and
-- add one to octave. 
--
-- __TODO__: We don't check for values out of range because we know what input we
-- are getting from the ark. Is this okay?
stdPitch :: RawPitch -> Pitch
stdPitch pitch1 = 
    let oldPnum = rawPnum pitch1
    in
    if oldPnum >= 0 && oldPnum < 7
        then Pitch {
            pnum      = toEnum $ rawPnum pitch1,
            oct       = rawOct pitch1,
            dur       = oldDur,
            accid     = oldAccid,
            accidType = oldAccidType
        }
        else Pitch { 
            pnum      = toEnum $ newPnum,
            oct       = newOct,
            dur       = oldDur,
            accid     = oldAccid,
            accidType = oldAccidType
        }
        where
            oldPnum      = rawPnum pitch1
            oldOct       = rawOct pitch1
            oldDur       = rawDur pitch1
            oldAccid     = rawAccid pitch1
            oldAccidType = rawAccidType pitch1

            newPnum     = snd pitchDivide
            newOct      = oldOct + fst pitchDivide
            pitchDivide = oldPnum `divMod` 7

-- | A 'RawPitch' is the same as a 'Pitch' except instead of a 'Pnum' it has
-- an 'Int'. This allows us to add and subtract from pitches and then use
-- 'stdPitch' to normalize them by adjusting the octave and pitch like the two
-- digits of a base-7 number.
data RawPitch = RawPitch {
    rawPnum    :: Int,
    rawOct     :: Int,
    rawDur     :: Dur,
    rawAccid   :: Accid,
    rawAccidType :: AccidType
} deriving (Show, Eq, Ord)

-- | Increment a pitch by increasing its 'Pnum' pitch number and its octave if
-- necessary (using 'Pitch' structure like a base-7 number). Create a new
-- 'Pitch' with incremented 'Pnum' and then standardize it with 'stdPitch' to
-- get correct octave and pitch number.
incPitch :: Pitch -> Int -> Pitch
incPitch pitch1 newPnum = stdPitch RawPitch {
    rawPnum      = fromEnum (pnum pitch1) + newPnum,
    rawOct       = oct pitch1,
    rawDur       = dur pitch1,
    rawAccid     = accid pitch1,
    rawAccidType = accidType pitch1
}


