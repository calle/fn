module Main where

import Aftonbladet
import Svd 
import Articles
import CountWords

main = do
  items <- aftonbladet 
  -- items <- sofies_mode
  -- items <- svd
  putStrLn $ unlines $ take 5 (map showArticleWordCount items)

showArticle :: Article -> String
showArticle article = show article

showArticleWordCount :: Article -> String
showArticleWordCount article = unlines $ ("== " ++ (title article)) : [ (show count) ++ " " ++ word | (word, count) <- (count_words (Articles.text article)) ]
