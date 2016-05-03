module Golf where

import Data.List
import Data.List.Split
import Data.Maybe
import Safe

skips :: [a] -> [[a]]
skips [] = []
skips x = map (f x) [1..(length x)]

f :: [a] -> Int -> [a]
f l n = catMaybes (map (headMay . reverse) (takeWhile ((n ==) . length) (chunksOf n l)))

localMaxima :: [Integer] -> [Integer]
localMaxima = catMaybes . map j . filter g . concat . map inits . tails

g :: [Integer] -> Bool
g (a:b:c:[])
  | a < b && b > c = True
g _ = False

j x = atMay x 1
