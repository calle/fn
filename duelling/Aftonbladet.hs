module Aftonbladet (aftonbladet, sofies_mode) where

import Articles
import Rss

aftonbladet :: IO [Article]
aftonbladet = fetch_articles "http://www.aftonbladet.se/rss.xml"

sofies_mode :: IO [Article]
sofies_mode = fetch_articles "http://www.aftonbladet.se/sofismode/rss.xml" 
