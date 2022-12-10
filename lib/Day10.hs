module Day10 (solve1, solve2) where

import Common.Parser (number)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

data Operation = Noop | AddX Int

solve1 :: String -> Int
solve1 s = sum $ map (signalStrengthAt cycles) [20, 60, 100, 140, 180, 220]
    where cycleOps = concatMap toCycles $ parseInput s
          cycles = scanl (\a b -> b a) 1 cycleOps

solve2 :: String -> String
solve2 s = map (cf . spriteVisibleAt cycles) [1..40] ++ "\n" ++
           map (cf . spriteVisibleAt cycles) [41..80] ++ "\n" ++
           map (cf . spriteVisibleAt cycles) [81..120] ++ "\n" ++
           map (cf . spriteVisibleAt cycles) [121..160] ++ "\n" ++
           map (cf . spriteVisibleAt cycles) [161..200] ++ "\n" ++
           map (cf . spriteVisibleAt cycles) [201..240]
    where cycleOps = concatMap toCycles $ parseInput s
          cycles = scanl (\a b -> b a) 1 cycleOps
          cf x = if x then '#' else '.'

signalStrengthAt :: [Int] -> Int -> Int
signalStrengthAt xs n = n * xs!!(n - 1)

spriteVisibleAt :: [Int] -> Int -> Bool
spriteVisibleAt xs n = mod (n-1) 40 == v || (mod n 40 == v) || (mod (n-2) 40 == v)
    where v = xs!!(n-1)

toCycles :: Operation -> [Int -> Int]
toCycles Noop = [id]
toCycles (AddX n) = [id, (+) n]

-- PARSER --

parseInput :: String -> [Operation]
parseInput s = fromRight [] $ parse input [] s

input :: GenParser Char st [Operation]
input = do
    endBy1 (noop <|> addx)  newline

noop :: GenParser Char st Operation
noop = do
    string "noop"
    return Noop

addx :: GenParser Char st Operation
addx = do
    string "addx "
    AddX <$> number
