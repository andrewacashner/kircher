{- |
 - Web input to arca
 - 2021/05/31
 -}

import Network.CGI as CGI

import Data.Maybe 
    (fromJust)

import System.FilePath
    (takeBaseName) 

import Arca_musarithmica 
    (arca)

import Lectio
    (
        arkMetadata,
        readInput,
        prepareInput,
        inputPhraseLengths
    )

import Fortuna
    (inputPerms)

import Scribo
    (compose)

selectInfile :: String -> String
selectInfile name = "input/" ++ filename
    where 
        filename = case name of
            "Ps150"                  -> "Ps-150.xml"
            "Boethius"               -> "Boethius-Nubibus_atris.xml"
            "Ave_maris_stella"       -> "Ave_maris_stella.xml"
            "Veni_creator_Spiritus"  -> "Veni_creator_Spiritus.xml"
            _ -> error $ "Unrecognized choice of input file " ++ name


-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
cgiMain :: CGIT IO CGIResult
cgiMain = do
   
    -- get input: choice of set texts (= input file)
    inputText <- CGI.getInput "inputText"

    let 
        infileName = selectInfile $ fromJust inputText
    
    rawInput  <- CGI.liftIO $ readFile infileName

    let 
        input     = Lectio.readInput rawInput
        sections  = prepareInput input 
        lengths   = inputPhraseLengths sections
        metadata  = arkMetadata input

    perms <- CGI.liftIO $ inputPerms lengths

    let 
        music = compose arca metadata sections perms 
--        ly_outfile = takeBaseName infileName ++ ".ly"
 
    CGI.output music


-- | Output Lilypond file for music set to input file.
main :: IO ()
main = CGI.runCGI $ CGI.handleErrors cgiMain


