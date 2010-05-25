module Svd where

import Articles
import Rss

main :: IO [Article]
main = fetch_articles "http://www.dn.se/m/rss/senaste-nytt"

