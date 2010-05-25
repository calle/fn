module Article where

import Time

data Article = Article {
  title :: String,
  date  :: Time,
  link  :: String,
  text  :: String
}