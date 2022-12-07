module Day5 (solve1, solve2) where

import Common.Parser (number)

import qualified Data.Sequence as Seq
import Data.Foldable
import Data.List
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

type Move = (Int, Int, Int)
type Stack = [Char]

solve1 :: String -> String
solve1 s = map head $ foldl (\a b -> b a) state (map (makeMove reverse) moves)
    where (state, moves) = parseInput s

solve2 :: String -> String
solve2 s = map head $ foldl (\a b -> b a) state (map (makeMove id) moves)
    where (state, moves) = parseInput s

makeMove :: (String -> String) -> Move -> [Stack] -> [Stack]
makeMove order (n, a, b) s = toList $ Seq.update (b-1) newTo $ Seq.update (a-1) newFrom seq
    where seq = Seq.fromList s
          newFrom = drop n $ s!!(a-1)
          toMove = order $ take n $ s!!(a-1)
          newTo = toMove ++ s!!(b-1)

-- PARSER --

parseInput :: String -> ([Stack], [Move])
parseInput s = fromRight ([], []) $ parse input [] s

input :: GenParser Char st ([Stack], [Move])
input = do
    s <- state
    newline
    ms <- endBy move newline
    return (s, ms)

state :: GenParser Char st [Stack]
state = do
    lines <- endBy1 line newline
    return $ map (filter (/= ' ')) $ transpose lines

line :: GenParser Char st [Char]
line = sepBy1 box (char ' ')

box :: GenParser Char st Char
box = do
    char '[' <|> char ' '
    result <- anyChar
    anyChar
    return result

move :: GenParser Char st Move
move = do
    many1 (letter <|> space)
    a <- number
    many1 (letter <|> space)
    b <- number
    many1 (letter <|> space)
    c <- number
    return (a, b, c)

