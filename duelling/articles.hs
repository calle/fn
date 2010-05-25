module Article where

import Data.Time.LocalTime

data Article = Article {
  title :: String,
  date  :: LocalTime,
  link  :: String,
  text  :: String
}