module Articles where

import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import Data.Maybe


type Title = String
type Link = String
type Content = String

data Article = Article {
  title :: Title,
  date  :: LocalTime,
  link  :: Link,
  text  :: Content
}
instance Show Article where
  show (Article title date link text) = unlines ["== " ++ title ++ " ==", (show date), text]

createArticle :: Title -> String -> Link -> Content -> Article
createArticle title dateStr link content = Article title date link content
  where date = fromJust (parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" dateStr)
