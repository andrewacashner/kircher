import Aedifico
    (ArkConfig (..),
     Style     (..),
     Meter     (..),
     Mode      (..))

import Lectio
    (prepareText)

data ArkInput = ArkInput {
    arkConfig :: ArkConfig,
    arkText   :: String
}

readText :: IO ArkInput
readText = do
    
    input <- readInput "test/input-simple.xml"

    text = prepareText $ arkText input

    return text
