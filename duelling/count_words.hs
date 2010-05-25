module CountWords where

import Articles

split_words :: String -> [String]
split_words "" = []
split_words x = ws : split_words s
    where (ws,s) = next_word x
 
next_word :: String -> (String, String)
next_word xs = next_word' ("", xs)
    where next_word' (a, x:xs)
              | xs == "" = (a ++ [x],[])
              | x == ' ' = (a,xs)
              | otherwise = next_word' (a ++ [x], xs)
