module Rss (fetch_articles) where 

import Articles
import Fetcher

fetch_articles :: String -> IO [Article]
fetch_articles url = do
  doc <- fetch url
  let items   = find doc "//channel/item"
  let articles = [ article ["./title", "./pubDate", "./link", "./description"] item | item <- items]
  return articles
    where article (t:d:l:c:_) i = createArticle (getText i t) (getText i d) (getText i l) (removeTags (getText i c))
          removeTags []    = ""
          removeTags (c:str) | c == '<'  = removeTags' str
                             | otherwise = c : removeTags str
          removeTags' [] = ""
          removeTags' (c:str) | c == '>'  = removeTags str
                              | otherwise = removeTags' str

