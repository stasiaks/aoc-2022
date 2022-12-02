module Day1 (solve1, solve2) where

import Common (getInput)

import Data.List

solve1 :: String -> Int
solve1 s = sumHighest 1 $ parse s

solve2 :: String -> Int
solve2 s = sumHighest 3 $ parse s

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn p xs = f xs []
    where f [] acc = [acc]
          f (y : ys) acc = if p == y
                           then acc : f ys []
                           else f ys $ acc ++ [y]

parse :: String -> [[Int]]
parse x = (map . map) read $ splitOn "" $ lines x

sumHighest :: Int -> [[Int]] -> Int
sumHighest n xs = sum $ take n $ (reverse . sort) $ map sum xs
