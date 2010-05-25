module Main where

import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Html.Pretty (content)
-- import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn (posInNewCxt, noPos)
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Xtract.Parse (xtract)
import Text.PrettyPrint.HughesPJ (render)

filename = "test.html"
doc = "<html><head><title>test</title></head><body><div id='test_id'>content</div></body></html>"

-- pattern syntax:
-- http://www.cs.york.ac.uk/fp/HaXml/Xtract.html

check :: String -> [String]
check pattern = map (render . content) $ xtract id pattern $ docContent noPos $ htmlParse filename doc
