module Golf where

import Safe
import Data.List.Split
import Data.Maybe

skips :: [a] -> [[a]]
skips [] = []
skips x = map (f x) [1..(length x)]

f :: [a] -> Int -> [a]
f l n = catMaybes (map (headMay . reverse) (takeWhile ((n ==) . length) (chunksOf n l)))
