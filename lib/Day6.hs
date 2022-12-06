module Day6 (solve1, solve2) where

import qualified Data.Set as Set

solve1 :: String -> Int
solve1 = findWindowEndWithDuplicates 4
solve2 :: String -> Int
solve2 = findWindowEndWithDuplicates 14

findWindowEndWithDuplicates :: Ord a => Int -> [a] -> Int
findWindowEndWithDuplicates a = f a a
    where f acc n xs =
            if n == length (Set.fromList (take n xs))
            then acc
            else f (acc + 1) n $ tail xs

