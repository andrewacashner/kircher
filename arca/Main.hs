module Main where

import Arca

main = do
--    putStrLn("perm2pitch:")
--    putStrLn(show (perm2pitch vperm 0 Soprano rperm 0 0 5))
--    putStrLn(show (perm2pitch vperm 1 Tenor rperm 1 3 3))
--
--    putStrLn("perm2Chorus:")
--    putStrLn(show (perm2Chorus vperm 0 rperm 0 1))
--
--    putStrLn("to lilypond:")
    putStrLn(lyScore [perm2Chorus vperm 0 rperm 0 1])
