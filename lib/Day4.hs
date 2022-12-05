module Day4 (solve1, solve2) where

import Common (getInput, splitOn, tuple)

import qualified Data.Set as Set
import Data.Char

type Range = (Int, Int)

solve1 :: String -> Int
solve1 s = length $ filter (uncurry fullyContains) $ parse s

solve2 :: String -> Int
solve2 s = length $ filter (uncurry overlaps) $ parse s

fullyContains :: Range -> Range -> Bool
fullyContains (xf, xt) (yf, yt)
    = (xf <= yf && xt >= yt)
    || (yf <= xf && yt >= xt)

overlaps :: Range -> Range -> Bool
overlaps (xf, xt) (yf, yt)
    = (xf <= yf && xt >= yf)
    || (yf <= xf && yt >= xf)

parse :: String -> [(Range, Range)]
parse x = map (tuple . map parseRange) $ parseToPairs x

parseToPairs :: String -> [[String]]
parseToPairs x = map (splitOn ',') $ lines x

parseRange :: String -> Range
parseRange x = tuple $ map read $ splitOn '-' x
    where f [x] = (x, 0)
          f [x, y] = (x, y)
          f _ = (0, 0)
