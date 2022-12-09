module Day9 (solve1, solve2) where

import Common.Parser (number)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

type Coord = (Int, Int)
type Positions = (Coord, Coord)

solve1 :: String -> Int
solve1 s = length $ Set.fromList $ map last $ scanl (\a b -> b a) initState es
    where ms = parseInput s
          fs = concatMap headMoves ms
          es = map executeMove fs
          initState = replicate 2 (0,0)

solve2 :: String -> Int
solve2 s = length $ Set.fromList $ map last $ scanl (\a b -> b a) initState es
    where ms = parseInput s
          fs = concatMap headMoves ms
          es = map executeMove fs
          initState = replicate 10 (0,0)

executeMove :: (Coord -> Coord) -> [Coord] -> [Coord]
executeMove m cs = foldl (\a b -> b a) (m (head cs) : tail cs) fs
    where fs = [adjustKnot x | x <- [1..(length cs - 1)] ]

headMoves :: HeadMove -> [Coord -> Coord]
headMoves (MRight n) = replicate n $ \(x, y) -> (x + 1, y)
headMoves (MLeft n)  = replicate n $ \(x, y) -> (x - 1, y)
headMoves (MUp n)    = replicate n $ \(x, y) -> (x, y + 1)
headMoves (MDown n)  = replicate n $ \(x, y) -> (x, y - 1)

adjustKnot :: Int -> [Coord] -> [Coord]
adjustKnot n cs = pcs ++ (nextKnotPosition cc h : ncs)
    where (pcs, cc : ncs) = splitAt n cs
          h = last pcs

nextKnotPosition :: Coord -> Coord -> Coord
nextKnotPosition (cx, cy) (hx, hy) | cx == hx && cy == hy = (cx, cy)
nextKnotPosition (cx, cy) (hx, hy) | cx == hx && (abs (cy - hy) == 1) = (cx, cy)
nextKnotPosition (cx, cy) (hx, hy) | cy == hy && (abs (cx - hx) == 1) = (cx, cy)
nextKnotPosition (cx, cy) (hx, hy) | (abs (cy - hy) == 1) && (abs (cx - hx) == 1) = (cx, cy)
nextKnotPosition (cx, cy) (hx, hy)  = (f cx hx, f cy hy)
    where f a b = case compare a b of
                  EQ -> a
                  LT -> a + 1
                  GT -> a - 1

-- PARSER --

data HeadMove = MRight Int | MLeft Int | MUp Int | MDown Int

parseInput :: String -> [HeadMove]
parseInput s = fromRight [] $ runParser input "/" [] s

input :: GenParser Char st [HeadMove]
input = do
    endBy1 move newline

move :: GenParser Char st HeadMove
move = do
    left <|> right <|> up <|> down

left :: GenParser Char st HeadMove
left = do
    try (char 'L')
    space
    MLeft <$> number

right :: GenParser Char st HeadMove
right = do
    try (char 'R')
    space
    MRight <$> number

up :: GenParser Char st HeadMove
up = do
    try (char 'U')
    space
    MUp <$> number

down :: GenParser Char st HeadMove
down = do
    try (char 'D')
    space
    MDown <$> number

