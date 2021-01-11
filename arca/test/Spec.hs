import System.Environment
import System.Process

import Data.Vector hiding (map, (++), concat)
import qualified Data.Vector as V (map, indexed)

import Data.List.Index as I (indexed)

import Arca_musarithmica
    (arca)

import Aedifico
import Cogito
import Scribo
    (pitch2ly)

-- Print out all voice permutations and rhythm permutations in order

vpermTable2pitches :: VpermTable -> Vector (Vector [Pitch])
vpermTable2pitches vpermTable = pitchVector
    where
        vpermChoirs  = vperms vpermTable
        pitchVector  = V.map (\vperm -> makePitch $ V.indexed vperm) vpermChoirs

        makePitch :: Vector (Int, [Int]) -> Vector [Pitch]
        makePitch v = V.map (\(i, ps) -> 
            map (\p -> stdPitch $ RawPitch (p - 1) (getOct i) Mn Na) ps) v
            where
                getOct :: Int -> Int
                getOct i | i >= 0 && i < 4 = [5, 4, 3, 2] !! i
                         | otherwise = error "octave index out of bounds"

vpermPrint :: Vector [Pitch] -> String
vpermPrint vperm = unlines
    ["\\score {"
     , "  <<"
     , "  \\new ChoirStaff"
     , "    <<"
     , "    \\new Staff"
     , "      <<"
     , "      \\new Voice {"
     , "        \\voiceOne \\clef \"treble\" \\time 2/2 "
     , "          " ++ ly ! 0
     , "      }"
     , "      \\new Voice { \\voiceTwo "
     , "          " ++ ly ! 1
     , "      }"
     , "      >>"
     , "    \\new Staff"
     , "      <<"
     , "      \\new Voice {"
     , "        \\voiceOne \\clef \"bass\" \\time 2/2 "
     , "          " ++ ly ! 2
     , "      }"
     , "      \\new Voice { \\voiceTwo "
     , "          " ++ ly ! 3
     , "      }"
     , "      >>"
     , "    >>"
     , "  >>"
     , "}"]
    where 
        ly = V.map (\v -> unwords $ map (\p -> pitch2ly p) v) vperm


main :: IO ()
main = do
    putStrLn "\nWriting Lilypond code..."
    writeFile "test/test.ly" lystring
    putStrLn "\nCalling Lilypond to make pdf..."
    callCommand "lilypond -o test/ test/test"
    putStrLn "\nOpening PDF..."
    callCommand "mupdf test/test.pdf"

    where   
        lystring = unlines $ map (\v -> unlines 
            ["\\version \"2.20.0\""
            , "\\book {"
            , "\\header {"
            , "  title=\"Arca musarithmica Athanasii Kircheri Societatis Iesu MDL\""
            , "  subtitle=\"Andreae Cashneri Universitatis Rochesterii electronice implementata MMXX\""
            , "}" 
            , unlines $ map (\(i, va) -> unlines 
                    ["\\bookpart { " 
                    , unlines $ map (\(j, vb) -> unlines 
                                ["\\markup { \\fill-line {"
                                 , "  \\center-column {"
                                 , "    \"PINAX " 
                                    ++ show (i + 1) 
                                    ++ " COLUMN " 
                                    ++ show (j + 1) 
                                    ++ "\""
                                 , "    }"
                                 , "  }"
                                 , "} "
                                , unlines vb]) $ I.indexed va
                    , "}"]) $ I.indexed v
            , "}"]) lylist

        lylist   = toList $ V.map (\v -> toList 
            $ V.map (\v -> toList $ V.map (\v -> toList v) v) v) lyvector

        lyvector = V.map (V.map (V.map (V.map vpermPrint))) pitches

        pitches  = V.map (V.map 
            (V.map (\c -> vpermTable2pitches $ colVpermTable c))) $ perms arca

{-
Arca
    perms: Vector (Syntagma)
            Vector (Pinax)
                Vector (Column)
                   colVpermTable: VpermTable
                    vperms: Vector (VpermChoir)
                        Vector (Vperm)
                            [Int]
vperms vpermTable :: Vector (Vector [Int])
perms arca :: Vector (Vector (Vector Column))
-}
