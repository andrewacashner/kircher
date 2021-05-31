{- |
 - Web input to arca
 - 2021/05/31
 -}

module Main where

import Network.CGI
import Text.XHtml

import System.Process 
    (callCommand)

import System.FilePath
    (
        dropExtension,
        takeBaseName
    ) 

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

inputForm = form << [ulist << 
                        [li << ("Name (required):" +++ textfield "name"),
                         li << ("Email (required):" +++ textfield "name"),
                             submit "" "Submit"]]

page t b = header << thetitle << t +++ body << b

cgiMain = do 
    mn <- getInput "name"
    let x = maybe inputForm greet mn
    output . renderHtml $ page "Input example" x

main = runCGI $ handleErrors cgiMain
-- | Get input text file, parse it, get number of random indices needed for
-- text, compose music for it using ark and write output.
--
-- Output to PDF via Lilypond.
main :: IO ()
main = do
   
    -- get input
    --  - choice of set texts (= input file), already uploaded
    [infileName] <- getArgs
    rawInput <- readFile infileName

    let 
        input     = readInput rawInput
        sections  = prepareInput input 
        lengths   = inputPhraseLengths sections
        metadata  = arkMetadata input

    perms <- inputPerms lengths

    let 
        music = compose arca metadata sections perms 

        ly_outfile = "./tmp/" ++ (takeBaseName infileName) ++ ".ly"
        lycommand = "lilypond -I ~/lib/ly -o " ++ (dropExtension ly_outfile)
 
    writeFile ly_outfile music
    callCommand lycommand




