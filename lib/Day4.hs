module Day4 (solve1, solve2) where

import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

type Range = (Int, Int)

solve1 :: String -> Int
solve1 s = length $ filter (uncurry fullyContains) $ parseInput s

solve2 :: String -> Int
solve2 s = length $ filter (uncurry overlaps) $ parseInput s

fullyContains :: Range -> Range -> Bool
fullyContains (xf, xt) (yf, yt)
    = (xf <= yf && xt >= yt)
    || (yf <= xf && yt >= xt)

overlaps :: Range -> Range -> Bool
overlaps (xf, xt) (yf, yt)
    = (xf <= yf && xt >= yf)
    || (yf <= xf && yt >= xf)

-- PARSER --

parseInput :: String -> [(Range, Range)]
parseInput s = fromRight [] $ parse input [] s

input :: GenParser Char st [(Range, Range)]
input = endBy rangePair newline

rangePair :: GenParser Char st (Range, Range)
rangePair = do
    one <- range
    char ','
    two <- range
    return (one, two)

range :: GenParser Char st Range
range = do
    one <- number
    char '-'
    two <- number
    return (one, two)

number :: GenParser Char st Int
number = do
    result <- many1 digit
    return $ read result

