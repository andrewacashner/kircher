{- |
 - Lectio: Read input text and determine parameters for musical setting
 -}

module Lectio where

import Data.Strings

cleanup :: String -> [String]
cleanup text = cleanText
    where
        cleanText = map removeEmpties noComments
        noComments = map (\ s -> removeComments s) text
        text = lines text

removeComments :: String -> String
removeComments str = takeWhile (/= '%') str

removeEmpties :: [String] -> [String]
removeEmpties text = filter (\ s -> (not . strNull) s) text
