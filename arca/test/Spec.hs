import Data.Vector hiding (map, (++), concat)
import qualified Data.Vector as V (map)

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
        pitchVector  = V.map (\vperm -> makePitch $ indexed vperm) vpermChoirs

        makePitch :: Vector (Int, [Int]) -> Vector [Pitch]
        makePitch v = V.map (\(i, ps) -> 
            map (\p -> Pitch (toEnum p) (getOct i) Mn Na) ps) v
            where
                getOct :: Int -> Int
                getOct i | i >= 0 && i < 4 = [4, 4, 3, 3] !! i
                         | otherwise = error "octave index out of bounds"

vpermPrint :: Vector [Pitch] -> String
vpermPrint vperm = 
    "\\score { << \\new ChoirStaff << \\new Staff <<\n\
    \\new Voice { \\voiceOne \\clef \"treble\" \\time 2/2 "
    ++ (concat $ map pitch2ly $ vperm ! 0)
    ++ "}\n\\new Voice { \\voiceTwo "
    ++ (concat $ map pitch2ly $ vperm ! 1)
    ++ "} >>\n\
    \\new Staff << \\new Voice { \\voiceOne \\clef \"bass\" \\time 2/2 "
    ++ (concat $ map pitch2ly $ vperm ! 2)
    ++ "} \\new Voice { \\voiceTwo "
    ++ (concat $ map pitch2ly $ vperm ! 3)
    ++ "} >> >> >> }"


main :: IO ()
main = putStrLn $ show lyvector
    where
        lyvector = V.map (V.map (map vpermPrint)) pitches
        pitches = V.map (V.map (V.map (\c -> vpermTable2pitches $ colVpermTable c))) $ perms arca

--   results     = vpermTable2pitches table
--   table       = colVpermTable column
--   column      = arkPerms ! 0 ! 0 ! 0
--   arkPerms    = perms arca

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
