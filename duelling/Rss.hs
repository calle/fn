module Rss (fetch_articles) where 

import Articles
import Fetcher
import Text.Regex (mkRegex, subRegex)


fetch_articles :: String -> IO [Article]
fetch_articles url = do
  doc <- fetch url
  let items   = find doc "//channel/item"
  let articles = [ createArticle
                    (getText item "./title")
                    (subRegex (mkRegex "GMT") (getText item "./pubDate") "+0000")
                    (getText item "./link")
                    (removeTags $ getText item "./description")
                    | item <- items]
  return articles
    where removeTags []    = ""
          removeTags (c:str) | c == '<'  = removeTags' str
                             | otherwise = c : removeTags str
          removeTags' [] = ""
          removeTags' (c:str) | c == '>'  = removeTags str
                              | otherwise = removeTags' str

