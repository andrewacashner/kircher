import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval

main :: IO ()
main = do
    let 
        xml = "<names><name>Andrew</name></names>"
        xmltree = head $ xread xml
        result = getXPath "//name" xmltree

    print result

