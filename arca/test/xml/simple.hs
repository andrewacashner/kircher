module Lectio where

import Data.String.Utils
    (strip)

import Data.Maybe
    (fromJust)

import Text.XML.Light

import Aedifico
    (ArkConfig (..),
     toStyle,
     toMeter,
     toMode)

-- | The input to the ark is an 'ArkConfig' element with mode, style, and
-- meter; and a list of strings, each of which will become a 'Sentence'
data ArkInput = ArkInput {
    arkConfig :: ArkConfig,
    arkText   :: [String]
} deriving Show

-- | Create a 'QName' to search the xml tree
xmlSearch :: String -> QName
xmlSearch s = QName {
    qName   = s,
    qURI    = Nothing,
    qPrefix = Nothing
}

-- | Get the text from a node
xmlNodeText :: 
    Element     -- ^ the node
    -> String   -- ^ element name
    -> String   -- ^ node text
xmlNodeText tree name = strContent $ fromJust element
    where
        element    = findElement searchName tree
        searchName = xmlSearch name

-- | Break text into strings at newlines, strip leading and trailing
-- whitespace, remove empty strings
cleanUpText :: String -> [String]
cleanUpText s = filter (not . null) $ map strip $ lines s


-- | Read an XML string and return the data for input to the ark ('ArkInput')
readInput :: String -> IO ArkInput
readInput s = do

    let 
        xml     = fromJust $ parseXMLDoc rawInput
         
        text    = cleanUpText $ xmlNodeText xml "text"
       
        xconfig  = fromJust $ findElement (xmlSearch "config") xml 
        settings = map (\ s -> fromJust $ findAttr (xmlSearch s) xconfig) 
                    ["style", "meter", "mode"]

        config = ArkConfig {
            arkStyle = toStyle $ settings !! 0,
            arkMeter = toMeter $ settings !! 1,
            arkMode =  toMode  $ settings !! 2
        }

        arkInput = ArkInput {
            arkConfig = config,
            arkText = text
        }

        return arkInput

