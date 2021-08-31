module Main where

import Text.XML.HXT.Core

note = mkelem "note" 
        [ sattr "dur" "4"
        , sattr "oct" "4"
        , sattr "pname" "c"
        ]
        [ selem "verse"
            [ mkelem "syl"
                [ sattr "con" "d"
                , sattr "wordpos" "i"
                ]
                [ txt "la" ]
            ]
        ]

main :: IO ()
main = do
    runX $ 
        root [] [note]
        >>>
        writeDocument [withIndent yes] "hello.xml"
    return ()

--note2mei :: ArrowXml a => Note -> a XmlTree XmlTree -- why??
--note2mei note = mkelem "note"
--            [ sattr "pname" meiPname
--            , sattr "oct"   meiOct
--            , sattr "dur"   meiDur
--            , sattr "dots"  meiDots
--            , sattr "accid" meiAccid
--            ]
--            [ selem "verse"
--                [ mkelem "syl"
--                    [ sattr "con" "d"
--                    , sattr "wordpos" meiWordpos
--                    ]
--                    [ txt meiSyl ]
--                ]
--            ]
--            where
--                pitch       = notePitch note
--                syllable    = noteSyllable note
--
--                meiPname       = pnum2mei   $ pnum pitch
--                meiOct         = oct2mei    $ oct pitch
--                meiDur         = dur2mei    $ dur pitch
--                meiDots        = dur2dots   $ dur pitch
--                meiAccid       = accid2mei  $ accid pitch
--                meiWordpos     = sylPos2mei $ sylPosition syllable 
--                meiSyl         = sylText syllable
--
--pnum2mei :: Pnum -> String
--pnum2mei p = [c] where c = "cdefgabc_" !! fromEnum p
--
--oct2mei :: Int -> String
--oct2mei = show
--
--dur2mei :: Dur -> String
--dur2mei d | d == DurNil             = "_"
--          | d `elem` [Br, BrD, BrR] = "breve"
--          | d `elem` [Sb, SbD, SbR] = "1"
--          | d `elem` [Mn, MnD, MnR] = "2"
--          | d `elem` [Sm, SmD, SmR] = "4"
--          | d `elem` [Fs, FsD, FsR] = "8"
--          | otherwise = error "Unknown duration"
--
--dur2dots :: Dur -> String
--dur2dots d | d `elem` [BrD, SbD, MnD, SmD, FsD] = "1"
--           | otherwise = "0"
--
--accid2mei :: Accid -> String
--accid2mei a = ["ff", "f", "n", "s", "ss", "_"] !! fromEnum a
--
--sylPos2mei :: SyllablePosition -> String
--sylPos2mei p | p == First              = "i"
--             | p == Last               = "t"
--             | p `elem` [Middle, Only] = "m"
--             | otherwise               = "_"
---- TODO is it okay to use "m" (medial) for individual syllables not part of a
---- larger word?
--
-- writeMEI :: [Note] -> String -> IO [XmlTree]
-- writeMEI notes outfileName = do
--     let xml = map note2mei notes
-- 
--     runX $
--         root [] [xml]
--         >>>
--         writeDocument [withIndent yes] outfileName


