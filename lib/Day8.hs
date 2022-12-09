module Day8 (solve1, solve2) where

import Data.Maybe
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Parsec (modifyState, getState)
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)

solve1 :: String -> Int
solve1 s = countVisible $ foldl (\a b -> b a) inMap
    [ checkVisibilityFrom (fc c) c | c <- Map.keys inMap,
                                     fc <- [ \(_, y) -> (-1, y)
                                           , \(_, y) -> (maxX + 1, y)
                                           , \(x, _) -> (x, -1)
                                           , \(x, _) -> (x, maxY + 1)] ]
    where inMap = parseInput s
          maxY = maximum $ map snd $ Map.keys inMap
          maxX = maximum $ map fst $ Map.keys inMap

solve2 :: String -> Int
solve2 s = maximum
    [ product
        [ countVisible $ markVisiblesFrom Nothing moveUp c inMap
        , countVisible $ markVisiblesFrom Nothing moveDown c inMap
        , countVisible $ markVisiblesFrom Nothing moveLeft c inMap
        , countVisible $ markVisiblesFrom Nothing moveRight c inMap] | c <- Map.keys inMap ]
    where inMap = parseInput s

countVisible :: Map Coord Tree -> Int
countVisible = length . Map.filter fst

checkVisibilityFrom :: Coord -> Coord -> Map Coord Tree -> Map Coord Tree
checkVisibilityFrom fc tc m = f (oneCloserTo tc fc)
    where f nc | fc == nc = visibleOnMap
          f nc = case Map.lookup nc m of
                    (Just (_, nh)) | nh >= fh -> m
                    _ -> f (oneCloserTo nc fc)
          (_,fh) = fromMaybe (False, 0) $ Map.lookup tc m
          visibleOnMap = Map.update (Just. visible) tc m

oneCloserTo :: Coord -> Coord -> Coord
oneCloserTo (ax, ay) (bx, by) = (f ax bx, f ay by)
    where f a b = case compare a b of
                  EQ -> a
                  LT -> a + 1
                  GT -> a - 1

markVisiblesFrom :: Maybe Int -> (Coord -> Coord) -> Coord -> Map Coord Tree -> Map Coord Tree
markVisiblesFrom mh mv cc x =
    case (mh, cv, nv) of
    (_, _, Nothing) -> x
    (Nothing, Just (_, ph), Just (_, nh)) | nh >= ph -> nx
    (Nothing, Just (_, h), _) -> markVisiblesFrom (Just h) mv nc nx
    (Just ch, _, Just (_, nh)) | nh >= ch -> nx
    _ -> markVisiblesFrom mh mv nc nx
    where nc = mv cc
          nv = Map.lookup nc x
          cv = Map.lookup cc x
          nx = Map.update (Just . visible) nc x

moveUp (x, y) = (x, y - 1)
moveDown (x, y) = (x, y + 1)
moveLeft (x, y) = (x - 1, y)
moveRight (x, y) = (x + 1, y)
visible (_, x) = (True, x)

-- PARSER --

type Coord = (Int, Int)
type Tree = (Bool, Int)

parseInput :: String -> Map Coord Tree
parseInput s = Map.map freshTree $ fromRight Map.empty $ parse map2d [] s

freshTree :: Int -> Tree
freshTree x = (False, x)

map2d :: GenParser Char st (Map Coord Int)
map2d = do
    ls <- endBy line newline
    return $ Map.fromList [ ((x, y), (ls!!y)!!x) | x <- [0..(length (head ls) - 1)],
                                                   y <- [0..(length ls - 1)] ]

line :: GenParser Char st [Int]
line = do
    ds <- many digit
    return $ map digitToInt ds
