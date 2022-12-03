module Day3 (solve1, solve2) where

import Common (getInput)

import qualified Data.Set as Set
import Data.Char


solve1 :: String -> Int
solve1 s = sum $ map (priority . findDuplicate . splitBag) $ parse s

solve2 :: String -> Int
solve2 s = sum $ map (priority . findDuplicate) $ splitGroups $ parse s

priority :: Char -> Int
priority x = if isUpper x
             then ord x - 38
             else ord x - 96

findDuplicate :: Ord a => [[a]] -> a
findDuplicate arr = head $ Set.toList $ foldl1 Set.intersection $ map Set.fromList arr

splitBag :: [a] -> [[a]]
splitBag arr = [take h arr, drop h arr]
    where h = length arr `div` 2

splitGroups :: [a] -> [[a]]
splitGroups [] = []
splitGroups xs = let (ys, zs) = splitAt 3 xs in ys : splitGroups zs

parse :: String -> [String]
parse = lines

