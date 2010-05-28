module Svd where

import Articles
import Rss

svd :: IO [Article]
svd = fetch_articles "http://www.dn.se/m/rss/senaste-nytt"

