module CountWords where

import Articles
import Data.List

count_words :: String -> [(String, Int)]
count_words x = count_words' (group_words x)

count_words' :: [[String]] -> [(String, Int)]
count_words' [] = []
count_words' (wl:wls) = (head wl, (length wl)) : (count_words' wls)

group_words :: String -> [[String]]
group_words x = group (sort ( split_words x))

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
