module Day1 (solve1, solve2) where

import Common (getInput, splitOn)

import Data.List

solve1 :: String -> Int
solve1 s = sumHighest 1 $ parse s

solve2 :: String -> Int
solve2 s = sumHighest 3 $ parse s

parse :: String -> [[Int]]
parse x = (map . map) read $ splitOn "" $ lines x

sumHighest :: Int -> [[Int]] -> Int
sumHighest n xs = sum $ take n $ (reverse . sort) $ map sum xs
