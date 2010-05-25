module CountWords where

import Articles
import Data.List

top_words :: Int -> String -> [String]
top_words x s = take x (top_words' (order_by_freq s))

top_words' :: [(String, Int)] -> [String]
top_words' [] = []
top_words' (wl:wls) = fst wl : top_words' wls

order_by_freq :: String -> [(String, Int)]
order_by_freq x = sortBy sorter (count_words x)
    where sorter (_,x1) (_,x2) = compare x2 x1
--sorter :: (String, Int) -> (String, Int) -> Ordering


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
