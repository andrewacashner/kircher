import Text.SXML

sxml = (note (attr [dur "4" oct "4" pname "c"]) 
            (verse (syl [attr [con "d" wordpos "i"] "la")))

main :: IO ()
main = do
    show $ renderString $ parseString sxml

