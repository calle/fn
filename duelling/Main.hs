module Main where

import Aftonbladet
import Svd 
import Articles
import CountWords
import List (sortBy)

items count = do
  items <- fetch_items
  putStrLn $ unlines $ take count (map showArticle (sortBy dateSort items))

wordCount count = do
  items <- fetch_items
  putStrLn $ unlines $ take count (map showArticleWordCount (sortBy dateSort items))

fetch_items :: IO [Article]
fetch_items = fetch_all [aftonbladet, sofies_mode, svd]
-- fetch_items = fetch_all [sofies_mode]

fetch_all :: [ IO [Article] ] -> IO [Article]
fetch_all [] = return []
fetch_all (x:xs) = do items <- x
                      rest <- fetch_all xs
                      return (items ++ rest)

dateSort :: Article -> Article -> Ordering
dateSort a b = compare (date b) (date a)

showArticle :: Article -> String
showArticle article = show article

showArticleWordCount :: Article -> String
showArticleWordCount article = unlines $ ("== " ++ (title article)) : [ (show count) ++ " " ++ word | (word, count) <- (count_words (Articles.text article)) ]
