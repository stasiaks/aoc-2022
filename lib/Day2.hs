module Day2 (solve1, solve2) where

import Common (getInput)

import Data.List

data Hand = Rock | Paper | Scissor
data Result = Lose | Draw | Win

solve1 :: String -> Int
solve1 s = sum $ map (countPoints . decrypt1) $ parse s

solve2 :: String -> Int
solve2 s = sum $ map (countPoints . decrypt2) $ parse s

countPoints :: (Hand, Result) -> Int
countPoints (x, y) = f1 x + f2 y
    where f1 Rock = 1
          f1 Paper = 2
          f1 Scissor = 3
          f2 Lose = 0
          f2 Draw = 3
          f2 Win = 6

parse :: String -> [(Char, Char)]
parse x = map f $ lines x
    where f (y : _ : z : _) = (y, z)

decrypt1 :: (Char, Char) -> (Hand, Result)
decrypt1 (x, y) = (p y, f (p x) (p y))
    where p 'X' = Rock
          p 'Y' = Paper
          p 'A' = Rock
          p 'B' = Paper
          p _ = Scissor
          f Rock Scissor = Lose
          f Rock Paper = Win
          f Paper Rock = Lose
          f Paper Scissor = Win
          f Scissor Paper = Lose
          f Scissor Rock = Win
          f _ _ = Draw

decrypt2 :: (Char, Char) -> (Hand, Result)
decrypt2 (x, y) = (f (p1 x) (p2 y), p2 y)
    where p1 'A' = Rock
          p1 'B' = Paper
          p1 _ = Scissor
          p2 'X' = Lose
          p2 'Y' = Draw
          p2 _ = Win
          f a Draw = a
          f Rock Win = Paper
          f Rock Lose = Scissor
          f Paper Win = Scissor
          f Paper Lose = Rock
          f Scissor Win = Rock
          f Scissor Lose = Paper
