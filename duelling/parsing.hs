module Main where

import Char
import Data.Maybe

import Network.HTTP
import Network.Browser
-- import Network.URI

import Text.HTML.TagSoup -- (parseTags, renderTags, sections, ~==)

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Html.Pretty (content)
-- import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn (posInNewCxt, noPos)
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Xtract.Parse (xtract)
import Text.PrettyPrint.HughesPJ (render)

-- doc = "<html><head><title>test</title></head><body><div id='test_id'>content</div></body></html>"

-- url = "http://www.svd.se/"
url = "http://mobil.dn.se/"

fetch :: String -> IO String
fetch url = do
  (uri, rsp) <- browse $ do
    setAllowRedirects True -- handle HTTP redirects
    setProxy (Proxy "http://192.168.0.103:3128" Nothing)
    request $ getRequest url
  getResponseBody $ Right rsp

--  rsp <- Network.HTTP.simpleHTTP (getRequest url)
--  fmap (take 100) (getResponseBody rsp)

-- pattern syntax:
-- http://www.cs.york.ac.uk/fp/HaXml/Xtract.html

convertWithTagSoup :: String -> String
convertWithTagSoup html = renderTags (parseTags html)

check :: String -> String -> [String]
check html pattern = map (render . content) $ xtract (map toLower) pattern $ docContent noPos $ htmlParse "local.html" html

parseXml :: String -> String
parseXml html = render . content $ docContent noPos $ xmlParse "local.html" html

parseHtml :: String -> String
parseHtml html = render . content $ docContent noPos $ htmlParse "local.html" html

test url = do
	html <- fetch url
	print $ check html "/html/*/h2/a/"

testSafe url pattern = do
	html <- fetch url
	print $ check (convertWithTagSoup html) pattern


tagsoup = do
	html <- fetch url
	putStrLn $ foldl (\a b -> a ++ ", " ++ b) "" (strings html)
	where 
		strings html = map renderTags $ partitions list_item (parseTags html)
		list_item (TagOpen name xs) = name == "p" && any (==True) [ a == "class" && b == "list_item" | (a,b) <- xs ]
		list_item _                 = False


