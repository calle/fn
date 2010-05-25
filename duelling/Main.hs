module Main where

import Articles
import Aftonbladet
import CountWords

main = do
  items <- sofies_mode
  return [count_words (Articles.text item) | item <- items]
