module CountWords where

import Articles

-- split_words :: String -> [String]
-- split_words x:xs
--     | x == ' ' = 


-- split_words' a x:xs
--     | xs == [] = 
--     | x == ' ' = split_words' (a ++ x) xs
--     | otherwise = split_words 

next_word :: String -> (String, String)
next_word xs = next_word' ("", xs)
    where next_word' (a, x:xs)
              | x == ' ' = (a,xs)
              | otherwise = next_word' (a ++ [x], xs)
                            
--next_word' :: (String, String) -> (String, String)
--n-ext_word' (a, x:xs)
--    | x == ' ' = (a,xs)
--    | otherwise = next_word' (a ++ [x], xs)