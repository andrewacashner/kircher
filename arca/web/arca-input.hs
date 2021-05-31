{- |
 - Web input to arca
 - 2021/05/31
 -}


{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5                 as H  
    hiding (main)
import Text.Blaze.Html5.Attributes      as HA
import Text.Blaze.Renderer.Utf8         as HR 
    (renderMarkup)
import qualified Data.ByteString.Lazy   as B 
import Network.CGI                      as CGI

-- import System.Process 
--     (callCommand)
-- 
-- import System.FilePath
--     (
--         dropExtension,
--         takeBaseName
--     ) 
-- 
-- import Arca_musarithmica 
--     (arca)
-- 
-- import Lectio
--     (
--         arkMetadata,
--         readInput,
--         prepareInput,
--         inputPhraseLengths
--     )
-- 
-- import Fortuna
--     (inputPerms)
-- 
-- import Scribo
--     (compose)

indexPage :: Markup
indexPage = H.docTypeHtml $ do
    H.head $ do
        H.title "Arca musarithmica"
        H.meta ! HA.charset "utf-8"
        -- H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "arca.css"
    H.body $ do
        H.section $ do
            H.h1 "Arca musarithmica Athanasii Kircherii MDCL"

            H.p $ do
                "A digital implementation of Athanasius Kircher's device for"
                " automatic music composition from his " 
                H.cite "Musurgia universalis" >> " (Rome, 1650), Book VIII."
            H.p $ do 
                "Implemented in Haskell by Andrew Cashner"
                " (Rochester, New York, 2021)."

            H.form $ do
                H.label ! HA.for "name" $ do
                    "Name"
                H.input ! HA.type_ "text" ! HA.id "name" ! HA.required "true"
                
cgiMain = do
    -- mn <- CGI.getInput "name"
    CGI.output . show $ HR.renderMarkup $ indexPage

main :: IO ()
main = CGI.runCGI $ CGI.handleErrors cgiMain

---- | Get input text file, parse it, get number of random indices needed for
---- text, compose music for it using ark and write output.
----
---- Output to PDF via Lilypond.
--main :: IO ()
--main = do
--   
--    -- get input
--    --  - choice of set texts (= input file), already uploaded
--    [infileName] <- getArgs
--    rawInput <- readFile infileName
--
--    let 
--        input     = readInput rawInput
--        sections  = prepareInput input 
--        lengths   = inputPhraseLengths sections
--        metadata  = arkMetadata input
--
--    perms <- inputPerms lengths
--
--    let 
--        music = compose arca metadata sections perms 
--
--        ly_outfile = "./tmp/" ++ (takeBaseName infileName) ++ ".ly"
--        lycommand = "lilypond -I ~/lib/ly -o " ++ (dropExtension ly_outfile)
-- 
--    writeFile ly_outfile music
--    callCommand lycommand
--
--
--
--
