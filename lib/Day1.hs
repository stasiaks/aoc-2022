module Day1 (solve1, solve2) where

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
input = sepBy section eol

section :: GenParser Char st [Int]
section = endBy number eol

number :: GenParser Char st Int
number = do
    result <- many1 digit
    return $ read result

eol = char '\n'
