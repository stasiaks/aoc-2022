module Day6 (solve1, solve2) where

import Common (getInput)

import qualified Data.Set as Set

solve1 :: String -> Int
solve1 = findWindowEndWithDuplicates 4 4
solve2 :: String -> Int
solve2 = findWindowEndWithDuplicates 14 14

findWindowEndWithDuplicates :: Ord a => Int -> Int -> [a] -> Int
findWindowEndWithDuplicates acc n xs =
    if n == length (Set.fromList (take n xs))
    then acc
    else findWindowEndWithDuplicates (acc + 1) n $ tail xs

parse :: String -> String
parse x = x
