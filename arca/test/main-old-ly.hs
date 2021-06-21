-- Output to PDF via Lilypond.
main :: IO ()
main = do
    
    [infileName, outfileName] <- getArgs
    rawInput <- readFile infileName

    let 
        input       = readInput rawInput
        sections    = prepareInput input 
        lengths     = inputPhraseLengths sections
        metadata    = arkMetadata input

    perms <- inputPerms lengths


  let
        music = compose arca metadata sections perms 

      ly_outfile = (dropExtension outfileName) ++ ".ly"
        lycommand = unwords ["lilypond -I ~/lib/ly -o", 
                             takeDirectory outfileName,
                             ly_outfile]
  writeFile ly_outfile music
  callCommand lycommand


