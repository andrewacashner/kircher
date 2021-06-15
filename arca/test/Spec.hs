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
    )

import qualified Data.Vector as V 
    (
        head,
        indexed,
        map, 
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
    writeFile "output/perms.ly" lySyntagmaI
    callCommand "lilypond -o output/ output/perms"
    where 
        lySyntagmaI = printAllPermsSyntagmaI $ perms arca

printAllPermsSyntagmaI :: Vector Syntagma -> String
printAllPermsSyntagmaI syntagmata = unlines
            ["\\version \"2.23.0\""
            , "\\paper { indent = 1.25\\in }"
            , "\\book {"
            , s1vpermString syntagmata
            , s1rpermString syntagmata
            , "}"]

vector2string :: Vector String -> String
vector2string = unlines . toList

s1vpermString :: Vector Syntagma -> String
s1vpermString syntagmata = vector2string $
     V.map (\(pinaxNum, pinakes) -> 
        unlines
            ["\\bookpart { " 
            , "  \\header {"
            , "    title=\"Arcae musarithmicae syntagma I\""
            , "    subtitle=\"Permutationes vocarum pinaci " ++ show pinaxNum ++ "\""
            , "  }"
            , vector2string $ V.map (\(colNum, columns) -> 
                vector2string $ V.map (\(permNum, vperms) -> 
                    s1vpermPrint vperms colNum permNum) $
                        V.indexed $
                            s1vpermTable2pitches $ colVpermTable columns) $ 
                    V.indexed pinakes 
            , "}"]) 
        $ V.indexed $ V.head syntagmata -- syntagma I only

s1rpermString :: Vector Syntagma -> String
s1rpermString syntagmata = vector2string $
    V.map (\syntagma -> vector2string $
        V.map (\(pinaxNum, pinakes) ->
            unlines
                ["\\bookpart {"
                , "  \\header {"
                , "    title=\"Arcae musarithmicae permutationes valorum metrorum\""
                , "    subtitle=\"PINAX " ++ show pinaxNum ++ "\""
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
                    $ V.indexed syntagma) syntagmata


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
     , "  \\new ChoirStaff \\with { instrumentName = \"vperm " ++ show permNum ++ "\" }"
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
    



