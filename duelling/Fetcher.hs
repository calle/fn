module Fetcher (fetch, find, getText, parse, text, showDoc, output) where

import Network.HTTP
import Network.Browser
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Pretty (content)
import Text.XML.HaXml.Types (Document)
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Util (docContent, tagTextContent)
import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Xtract.Parse (xtract)
import Text.XML.HaXml.Types (Content)
import Data.Encoding
import Data.Encoding.UTF8

import Char

fetch :: String -> IO (Content Posn)
fetch url = do
    content <- download url
    return (parse content)

find :: Content Posn -> String -> [Content Posn]
find doc pattern = xtract id pattern doc

getText :: Content Posn -> String -> String
getText doc pattern = text $ head $ find doc pattern

text :: Content Posn -> String
text content = tagTextContent content

parse :: String -> Content Posn
parse xml = docContent noPos $ xmlParse "downloaded.xml" xml

download :: String -> IO String
download url = do
  (uri, rsp) <- browse $ do
    setAllowRedirects True
    -- setProxy (Proxy "http://192.168.0.103:3128" Nothing)
    setOutHandler (\s -> return ())
    request $ getRequest url
  fmap (decodeString UTF8) (getResponseBody $ Right rsp)

showDoc :: Content Posn -> String
showDoc doc = render $ content $ doc

output :: IO (Content Posn) -> IO ()
output doc = do 
    html <- doc
    putStrLn $ showDoc html
