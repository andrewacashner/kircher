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
vpermTable2pitches vpermTable = vpermVector
    where
        vpermChoirs  = vperms vpermTable
        vpermVector  = V.map (\vperm -> makePitch $ V.indexed vperm) vpermChoirs

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

rpermTable2pitches :: RpermTable -> Vector (Vector [Pitch])
rpermTable2pitches table = vpermVector
    where
        vpermVector = V.map (\meter -> 
            V.map (\v -> map (\dur -> Pitch PCc 5 dur Na) v) 
            $ rperms meter) table

-- TODO add meters
rpermPrint :: Vector [Pitch] -> String
rpermPrint rperm = "{ " ++ ly ++ " }"
    where
        ly = unwords $ toList $ V.map (\v -> unwords $ map (\p -> pitch2ly p) v) rperm

main :: IO ()
main = do
    writeFile "test/perms.ly" vpermString
    callCommand "lilypond -o test/ test/perms"

    where   
        ly = unlines [vpermString, rpermString]
        vpermString = unlines $ map (\v -> unlines 
            ["\\version \"2.20.0\""
            , "\\book {"
            , "\\header {"
            , "  title=\"Arca musarithmica Athanasii Kircheri Societatis Iesu MDL\n" ++
            "Andreae Cashneri Universitatis Rochesterii electronice implementata MMXXI\""
            , "  subtitle=\"Permutationes vocarum\""
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
            , "}"]) vpermList

        vpermList   = toList $ V.map (\v -> toList 
            $ V.map (\v -> toList $ V.map (\v -> toList v) v) v) vpermVector

        vpermVector = V.map (V.map (V.map (V.map vpermPrint))) pitches

        pitches  = V.map (V.map 
            (V.map (\c -> vpermTable2pitches $ colVpermTable c))) $ perms arca


        rpermString = unlines $ map (\r -> unlines
            ["\\version \"2.20.0\""
            , "\\book {"
            , "\\header {"
            , "  title=\"Arca musarithmica Athanasii Kircheri Societatis Iesu MDL\n" ++
            "Andreae Cashneri Universitatis Rochesterii electronice implementata MMXXI\""
            , "  subtitle=\"Permutationes valorum metrarum\""
            , "}" 
            , unlines $ map (\(i, ra) -> unlines 
                ["\\bookpart { "
                , unlines $ map (\(j, rb) -> unlines 
                    ["\\markup { \\fill-line { \\center-column { \"PINAX " 
                        ++ show (i + 1) 
                        ++ ", COLUMN "
                        ++ show (j + 1)
                        ++ "\" } } }"
                    , unlines rb]) $ I.indexed ra
                , "}"]) $ I.indexed r
            , "}"]) rpermList

        rpermList = toList $ V.map (\v -> toList
            $ V.map (\v -> toList $ V.map (\v -> toList v) v) v) rpermVector

        rpermVector = V.map (V.map (V.map (V.map rpermPrint))) rperms

        rperms = V.map (V.map
            (V.map (\c -> rpermTable2pitches $ colRpermTable c))) $ perms arca

{-
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
