module Golf where

import Data.List
import Data.List.Split
import Data.Maybe
import Safe

-- I chose not to reduce the variable names

skips :: [a] -> [[a]]
skips [] = []
skips x = map (genSkips x) [1..(length x)]

genSkips :: [a] -> Int -> [a]
genSkips list size = catMaybes $ map (headMay . reverse) $ takeWhile ((size ==) . length) $ chunksOf size list

localMaxima :: [Integer] -> [Integer]
localMaxima = catMaybes . map getMiddleElem . filter hasLocalMax . concat . map inits . tails

hasLocalMax :: [Integer] -> Bool
hasLocalMax (a:b:c:[])
  | a < b && b > c = True
hasLocalMax _ = False

getMiddleElem trio = atMay trio 1

histogram :: [Integer] -> String
histogram a = (unlines $ reverse $ filter (/= "          ") $ map (histLine $ map (length . k) [0..9]) [0..(length a) - 1]) ++ "==========\n0123456789\n" 
  where
    k :: Integer -> [Int]
    k x = elemIndices x a
    histLine scores height = concat $ map (isAsterisk height) scores
    isAsterisk h x = if x > h then "*" else " "
