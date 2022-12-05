module Day5 (solve1, solve2) where

import Common (getInput, splitOn)

import qualified Data.Sequence as Seq
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe

type Move = (Int, Int, Int)
type Stack = [Char]
type State = [Stack]

solve1 :: String -> String
solve1 s = map head $ foldl (\a b -> b a) state (map (move reverse) moves)
    where (state, moves) = parse s

solve2 :: String -> String
solve2 s = map head $ foldl (\a b -> b a) state (map (move id) moves)
    where (state, moves) = parse s

move :: (String -> String) -> Move -> State -> State
move order (n, a, b) s = toList $ Seq.update (b-1) newTo $ Seq.update (a-1) newFrom seq
    where seq = Seq.fromList s
          newFrom = drop n $ s!!(a-1)
          toMove = order $ take n $ s!!(a-1)
          newTo = toMove ++ s!!(b-1)

parse :: String -> (State, [Move])
parse x = (parseState (head xs), map parseStep (xs!!1))
    where xs = splitOn "" $ lines x

parseState :: [String] -> State
parseState x = map (filter (/= ' ')) $ transpose $  map parseStateRow x

parseStateRow :: String -> String
parseStateRow x = toList $ fmap flat $ Seq.chunksOf 4 $ Seq.fromList x
    where flat s = fromMaybe ' ' $ Seq.lookup 0 $ Seq.filter (\x -> x /= ' ' && x /= '[') s

parseStep :: String -> Move
parseStep x = (read (xs!!1), read (xs!!3), read (xs!!5))
    where xs = splitOn ' ' x
