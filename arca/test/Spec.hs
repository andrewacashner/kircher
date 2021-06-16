{-
 - name:        Spec.hs 
 - description: Print out all voice permutations and rhythm permutations in
 -              order
 - date:        2021/01/12
 -}

module Main where

import System.Process
    (callCommand)

import Data.Vector hiding 
    (
        (++), 
        concat,
        head,
        map, 
        zip
    )

import qualified Data.Vector as V 
    (
        head,
        indexed,
        map, 
        toList,
    )

import Data.List.Index as I 
    (indexed)

import Arca_musarithmica
    (arca)

import Aedifico

import Cogito

import Scribo
    (pitch2ly)

main :: IO ()
main = do
    writeFile "output/test.ly" ly
    callCommand "lilypond -I ~/lib/ly -o output/ output/test"
    where 
        ly          = printLyFrame $ lySyntagma1 ++ lySyntagma2
        lySyntagma1 = "" -- printAllPermsSyntagma1 $ perms arca
        lySyntagma2 = printAllPermsSyntagma2 $ perms arca

vector2string :: Vector String -> String
vector2string = unlines . toList

printLyFrame :: String -> String
printLyFrame contents = unlines
    ["\\version \"2.23.0\""
    ,"\\include \"early-music.ly\""
    ,"\\include \"mensurstriche.ly\""
    , "\\paper { indent = 1.25\\in }"
    , "\\book {"
    , contents
    , "}"]

-- * SYNTAGMA I
printAllPermsSyntagma1 :: Vector Syntagma -> String
printAllPermsSyntagma1 syntagmata = vperms ++ rperms
    where
        vperms = s1vpermString syntagmata
        rperms = s1rpermString syntagmata


-- | Get the pitch numbers for each set of 4-voice permutations, convert them
-- to pitches all with the same rhythms, octaves spaced out, on two staves
s1vpermString :: Vector Syntagma -> String
s1vpermString syntagmata = vector2string 
    $ V.map (\(pinaxNum, pinakes) -> 
         unlines
             ["\\bookpart { " 
             , "  \\header {"
             , "    title=\"Arcae musarithmicae syntagma I\""
             , "    subtitle=\"Permutationes vocarum pinaci " ++ show (pinaxNum + 1) ++ "\""
             , "  }"
             , vector2string 
                $ V.map (\(colNum, columns) -> vector2string 
                    $ V.map (\(permNum, vperms) -> 
                        s1vpermPrint vperms colNum permNum) 
                    $ V.indexed 
                        $ s1vpermTable2pitches $ colVpermTable columns) 
                $ V.indexed pinakes 
             , "}"]) 
     $ V.indexed $ V.head syntagmata
     -- only do syntagma I (index 0)
     
-- | Get all the rhythm permutations, print them all with the same pitch
s1rpermString :: Vector Syntagma -> String
s1rpermString syntagmata = vector2string $
    V.map (\(pinaxNum, pinakes) ->
        unlines
            ["\\bookpart {"
            , "  \\header {"
            , "    title=\"Arcae musarithmicae permutationes valorum metrorum\""
            , "    subtitle=\"PINAX " ++ show (pinaxNum + 1) ++ "\""
            , "  }"
            , vector2string 
                $ V.map (\(colNum, columns) ->
                        (unlines. toList) 
                            $ V.map (\(meterNum, meters) ->
                                    s1rpermPrint meters colNum meterNum) 
                                        $ V.indexed 
                                            $ s1rpermTable2pitches 
                                                $ colRpermTable columns) 
                    $ V.indexed pinakes
            , "}"])
    $ V.indexed $ V.head syntagmata
     -- only do syntagma I (index 0)


s1vpermTable2pitches :: VpermTable -> Vector (Vector [Pitch])
s1vpermTable2pitches table = vpermVector
    where
        vpermVector  = V.map (\vperm -> makePitches $ V.indexed vperm) vpermChoirs
        vpermChoirs  = vperms table 

        makePitches :: Vector (Int, [Int]) -> Vector [Pitch]
        makePitches v = V.map (\(i, ps) -> map (\p -> makePitch i p) ps) v

        makePitch :: Int -> Int -> Pitch
        makePitch i p = stdPitch $ RawPitch {
                rawPnum  = p - 1,
                rawOct   = [5, 4, 3, 2] !! i,
                rawDur   = Mn,
                rawAccid = Na
        }


s1vpermPrint :: Vector [Pitch] -> Int -> Int -> String
s1vpermPrint vperm colNum permNum  = unlines
    ["\\score {"
     , if permNum == 0 
        then "\\header { piece=\"COLUMN " ++ show colNum ++ "\" }"
        else ""
     , "  <<"
     , "  \\new StaffGroup \\with { instrumentName = \"vperm " ++ show permNum ++ "\" }"
     , "    <<"
     , "    \\new Staff"
     , "      <<"
     , "      \\new Voice { "
        ++ "\\voiceOne \\clef \"treble\" \\time 2/2 "
        ++ ly ! 0
        ++ "}"
     , "      \\new Voice { \\voiceTwo "
        ++ ly ! 1
        ++ "}"
     , "      >>"
     , "    \\new Staff"
     , "      <<"
     , "      \\new Voice { "
        ++ "\\voiceOne \\clef \"bass\" \\time 2/2 "
        ++ ly ! 2
        ++ "}"
     , "      \\new Voice { \\voiceTwo "
        ++ ly ! 3
        ++ "}"
     , "      >>"
     , "    >>"
     , "  >>"
     , "}"]
    where 
        ly = V.map (\v -> unwords $ map (\p -> pitch2ly p) v) vperm

s1rpermTable2pitches :: RpermTable -> Vector (Vector [Pitch])
s1rpermTable2pitches table = vpermVector
    where
        vpermVector = V.map (\meter -> 
                            V.map (\rpermChoir -> 
                                (map makePitch $ V.head rpermChoir))
                                -- for Syntagma 1 there is only one element in
                                -- each rpermChoir
                            $ rperms meter) 
                        table

        makePitch :: Dur -> Pitch
        makePitch dur = Pitch PCc 5 dur Na

s1rpermPrint :: Vector [Pitch] -> Int -> Int -> String
s1rpermPrint meters colNum meterNum = vector2string $ 
    V.map (\(permNum, perm) -> 
        unlines
            ["\\score {"
            , if meterNum == 0 && permNum == 0 
                then
                    "  \\header { piece=\"COLUMN " ++ show colNum ++ "\" }"
                else ""
            , if permNum == 0 
                then
                    "  \\header { opus=\""
                        ++ ["Dupla", "Tripla maior", "Tripla minor"] !! meterNum 
                        ++ "\" }"
                else ""
            , "  <<"
            , "  \\new Staff \\with { "
                ++ "instrumentName = \"rperm " ++ show permNum ++ "\" }"
            , "    <<"
            , "    \\new Voice {"
            , "      \\time " ++ ["2/2 ", "3/1 ", "3/2 "] !! meterNum
            , unwords $ map (\pitch -> pitch2ly pitch) perm
            , "    }"
            , "    >>"
            , "  >>"
            , "}"]) $
        V.indexed meters
    

-- * SYNTAGMA II
-- | Get the four-voice permutations of voices AND of rhythms, match them up
-- and make pitches from them, and print on two staves
printAllPermsSyntagma2 :: Vector Syntagma -> String
printAllPermsSyntagma2 syntagmata = vector2string 
    $ V.map (\(pinaxIndex, pinax) ->
        unlines
            ["\\bookpart { " 
            , "  \\header {"
            , "    title=\"Arcae musarithmicae syntagma II\""
            , unwords ["    subtitle=\"Permutationes vocarum pinaci",
                       show (pinaxIndex + 1),
                       "\""]
            , "  }"
            , vector2string 
                $ V.map (\(columnIndex, column) -> 
                    s2column2pitches column columnIndex) 
            $ V.indexed pinax
            , "}"]) 
        $ V.indexed $ syntagmata ! 1 -- syntagma II only
       
-- __TODO__ This does not account for rests!
-- We need to do as we did in Cogito (ark2voice, using zipFill) to stitch
-- pitch numbers and durations together into pitch list
s2column2pitches :: Column -> Int -> String
s2column2pitches column colNum = unwords $
    map (\(permIndex, perm) ->
        s2vpermPrint perm colNum permIndex)
    $ I.indexed pitchList
    where
        pitchList = map (\perm -> 
                        (map (\(voiceIndex, voice) ->
                            (map (\(dur, pnum) -> 
                                makePitch voiceIndex pnum dur) 
                            voice))
                        $ I.indexed perm))
                     $ s2column2permPairs column

        makePitch :: Int    -- ^ voice index
                     -> Int -- ^ pitch number
                     -> Dur 
                     -> Pitch
        makePitch voiceIndex pitchNum dur = stdPitch $ RawPitch {
            rawPnum     = pitchNum - 1,
            rawOct      = [5, 4, 3, 2, 1] !! voiceIndex,
            rawDur      = dur,
            rawAccid    = Na
        }
       
s2column2permPairs :: Column -> [[[(Dur, Int)]]]
s2column2permPairs column = permPairs
    where
        vpermTable = colVpermTable column
        rpermTable = V.head $ colRpermTable column
            -- only 1 meter in syntagma II

        vpermChoir = V.toList $ V.map V.toList $ vperms vpermTable
        rpermChoir = V.toList $ V.map V.toList $ rperms rpermTable

        permChoirPairs  = zip rpermChoir vpermChoir         -- e.g., (vperm 1, rperm 1)
        permListPairs   = map zipPair permChoirPairs        -- (Soprano, Soprano)

        -- Combine sublists of pitch numbers and durations, 
        -- inserting Rests where there are Rest durations
        permPairs       = map (map (\(dur, pnum) -> 
                            zipFill dur pnum isRest $ fromEnum Rest))
                          permListPairs

zipPair :: ([a], [b]) -> [(a, b)]
zipPair pair = zip (fst pair) (snd pair)

s2vpermPrint :: [[Pitch]] -> Int -> Int -> String
s2vpermPrint voiceList colNum permNum = unlines
    ["\\score {"
     , if permNum == 0 
        then "\\header { piece=\"COLUMN " ++ show colNum ++ "\" }"
        else ""
     , "  <<"
     , "  \\new StaffGroup \\with { instrumentName = \"perm " ++ show permNum ++ "\" }"
     , "    <<"
     , "    \\new Staff"
     , "      <<"
     , "      \\new Voice { "
        ++ "\\voiceOne \\clef \"treble\" \\time 2/2 "
        ++ ly !! 0
        ++ "\\FinalBar }"
     , "      \\new Voice { \\voiceTwo "
        ++ ly !! 1
        ++ "\\FinalBar }"
     , "      >>"
     , "    \\new Staff"
     , "      <<"
     , "      \\new Voice { "
        ++ "\\voiceOne \\clef \"bass\" \\time 2/2 "
        ++ ly !! 2
        ++ "\\FinalBar }"
     , "      \\new Voice { \\voiceTwo "
        ++ ly !! 3
        ++ "\\FinalBar }"
     , "      >>"
     , "    >>"
     , "  >>"
     , "}"]
    where 
        ly = map (\v -> unwords $ map pitch2ly $ stepwise v) voiceList



