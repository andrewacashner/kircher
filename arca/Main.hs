module Main where

import Arca

main = do
    putStrLn(show (perm2pitch vperm 0 Soprano rperm 0 0 5))
    putStrLn(show (perm2pitch vperm 1 Tenor rperm 1 3 3))
