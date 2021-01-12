{-
 - name:        Spec.hs 
 - description: Print out all voice permutations and rhythm permutations in
 -              order
 - date:        2021/01/12
 -}

module Main where

import System.Environment
import System.Process

import Data.Vector hiding 
    (map, (++), concat)

import qualified Data.Vector as V 
    (map, indexed)

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
    writeFile "test/perms.ly" ly
    callCommand "lilypond -o test/ test/perms"
    where 
        ly = printAllPerms $ perms arca

printAllPerms :: Vector Syntagma -> String
printAllPerms syntagmata = unlines
            ["\\version \"2.20.0\""
            , "\\paper { indent = 1.25\\in }"
            , "\\book {"
            , vpermString syntagmata
            , rpermString syntagmata
            , "}"]

vector2string :: Vector String -> String
vector2string = unlines . toList

vpermString :: Vector Syntagma -> String
vpermString syntagmata = vector2string $ 
    V.map (\syntagma -> vector2string $
     V.map (\(pinaxNum, pinakes) -> 
        unlines
            ["\\bookpart { " 
            , "  \\header {"
            , "    title=\"Arcae musarithmicae permutationes vocarum\"" 
            , "    subtitle=\"PINAX " ++ show pinaxNum ++ "\""
            , "  }"
            , vector2string $ V.map (\(colNum, columns) -> 
                vector2string $ V.map (\(permNum, vperms) -> 
                    vpermPrint vperms colNum permNum) $
                        V.indexed $
                            vpermTable2pitches $ colVpermTable columns) $ 
                    V.indexed pinakes 
            , "}"]) $ 
        V.indexed syntagma) syntagmata

rpermString :: Vector Syntagma -> String
rpermString syntagmata = vector2string $
    V.map (\syntagma -> vector2string $
        V.map (\(pinaxNum, pinakes) ->
            unlines
                ["\\bookpart {"
                , "  \\header {"
                , "    title=\"Arcae musarithmicae permutationes valorum metrorum\""
                , "    subtitle=\"PINAX " ++ show pinaxNum ++ "\""
                , "  }"
                , vector2string $ V.map (\(colNum, columns) ->
                    (unlines. toList) $ V.map (\(meterNum, meters) ->
                       rpermPrint meters colNum meterNum) $
                        V.indexed $
                            rpermTable2pitches $ colRpermTable columns) $
                    V.indexed pinakes
                , "}"]) $
            V.indexed syntagma) syntagmata


vpermTable2pitches :: VpermTable -> Vector (Vector [Pitch])
vpermTable2pitches table = vpermVector
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


vpermPrint :: Vector [Pitch] -> Int -> Int -> String
vpermPrint vperm colNum permNum  = unlines
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

rpermTable2pitches :: RpermTable -> Vector (Vector [Pitch])
rpermTable2pitches table = vpermVector
    where
        vpermVector = V.map (\meter -> V.map (map makePitch) $ rperms meter) table

        makePitch :: Dur -> Pitch
        makePitch dur = Pitch PCc 5 dur Na

rpermPrint :: Vector [Pitch] -> Int -> Int -> String
rpermPrint meters colNum meterNum = vector2string $ 
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
    


{-
 - Structure of the Ark

Arca
    vperms: Vector (Syntagma)
            Vector (Pinax)
                Vector (Column)
                   colVpermTable: VpermTable
                    vperms: Vector (VpermChoir)
                        Vector (Vperm)
                            [Int]

        vperms vpermTable :: Vector (Vector [Int])
        perms arca :: Vector (Vector (Vector Column))

    rperms:
    Arca: vperms :: Vector (Syntagma)
    Syntagma : Vector (Pinax)  
    Pinax: Vector (Column)
    Column: colRpermTable :: RpermTable
    RpermTable :: Vector (RpermMeter)
    RpermMeter: rperms :: Vector (Rperm)
    Rperm :: [Dur]

        rperms = colRpermTable (perms arca ! syntagma ! pinax ! column) ! meter ! index :: [Dur]
-}
