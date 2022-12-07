module Day1 (solve1, solve2) where

import Common.Parser (number)

import Data.List
import Text.ParserCombinators.Parsec
import Data.Either (fromRight)

solve1 :: String -> Int
solve1 s = sumHighest 1 $ parseInput s

solve2 :: String -> Int
solve2 s = sumHighest 3 $ parseInput s

sumHighest :: Int -> [[Int]] -> Int
sumHighest n xs = sum $ take n $ (reverse . sort) $ map sum xs

-- PARSER --

parseInput :: String -> [[Int]]
parseInput s = fromRight [] $ parse input [] s

input :: GenParser Char st [[Int]]
input = sepBy section newline

section :: GenParser Char st [Int]
section = endBy number newline

