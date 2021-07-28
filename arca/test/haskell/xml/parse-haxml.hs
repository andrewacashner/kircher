import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.XmlContent.Parser

data Names = Names {
    roster :: [Name]
} deriving Show

data Name = Name {
    first  :: String,
    middle :: String,
    last   :: String
} deriving Show

data Favorite = Favorite {
    favType  :: String,
    favThing :: String
} deriving Show

instance HTypeable Names where
    toHType (Names nn) = Defined "names" [] [Constr "names" [] [toHType nn]]

instance HTypeable Name where
    toHType n =
        let Name first middle last = n
        in Defined "name" [] [Constr "name" [] 
                                [toHType first, toHType middle, toHType last]]

-- instance HTypeable Favorite where
--     toHType (Favorite type thing) =
--         Defined "favorite" [] [Constr "favorite" [] [toHType thing]]

instance XmlContent Names where
    parseContents = inElement "names" (Names <$> parseContents)

    toContents v@(Names nn) =
        [mkElemC (showConstr 0 $ toHType v) (toContents nn)]

instance XmlContent Name where
    parseContents = inElement "name" (Name <$> parseFirst <*> parseMiddle <*> parseLast)
        where
            parseFirst  = inElement "first" text
            parseMiddle = inElement "middle" text
            parseLast   = inElement "last" text

    toContents a@(Name first middle last) =
        [mkElemC (showConstr 0 $ toHType a) 
            [mkElemC "first"  $ toText first,
             mkElemC "middle" $ toText middle,
             mkElemC "last"   $ toText last]]
                                     
main :: IO ()
main = do
    names <- fReadXml "input.xml" :: IO Names
    let msg = "Names:\n\n" ++ (unwords (map (\ n -> first n ++ middle n ++ Main.last n ++ "\n") (roster names)))
    putStrLn msg
