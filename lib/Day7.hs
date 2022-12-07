module Day7 (solve1, solve2) where

import Common (splitOn)
import Common.Parser (number)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Either (fromRight)
import Text.Parsec.Prim (modifyState, getState)
import Text.ParserCombinators.Parsec

data File = Dir String | File String Int
    deriving (Show)

solve1 :: String -> Int
solve1 s = Map.foldr (+) 0 $ Map.filter (<= 100000) sizeMap
    where sizeMap = foldl (\a b -> b a) Map.empty (fs (parseInput s))
          fs = map (flip addSizes)

solve2 :: String -> Int
solve2 s = minimum $ Map.elems $ Map.filter (>= missingSpace) sizeMap
    where sizeMap = foldl (\a b -> b a) Map.empty (fs (parseInput s))
          fs = map (flip addSizes)
          fillSize = maximum $ Map.elems sizeMap -- Root
          unusedSpace = 70000000 - fillSize
          missingSpace = 30000000 - unusedSpace

addSizes :: Map String Int -> File -> Map String Int
addSizes m (Dir _) = m
addSizes m (File fn s) = foldl (\a b -> b a) m fs
    where fs = map (\x -> Map.insertWith (+) x s) (init(allPaths fn))

allPaths :: String -> [String]
allPaths p = f (splitOn '/' p) []
    where f [] acc = acc
          f ps acc = f (init ps) (intercalate "/" ps : acc)

-- PARSER --

data DirectoryChange = DirUp | DirDown String
    deriving (Show)
parseInput :: String -> [File]
parseInput s = fromRight [] $ runParser input "/" [] s

input :: GenParser Char String [File]
input = do
    fls <- many moveWithFileList
    return $ concat fls

moveWithFileList :: GenParser Char String [File]
moveWithFileList = do
    endBy1 cd newline
    string "$ ls"
    newline
    endBy (dir <|> file) newline

cd :: GenParser Char String DirectoryChange
cd = do
    try (string "$ cd ")
    path <- many1 $ noneOf "\n"
    let result = (if path == ".." then DirUp else DirDown path)
    modifyState (updatePath result)
    return result

updatePath :: DirectoryChange -> String -> String
updatePath DirUp p = intercalate "/" (init (splitOn '/' (init p))) ++ "/"
updatePath (DirDown np) p = p ++ np ++ "/"

dir :: GenParser Char String File
dir = do
    try (string "dir ")
    name <- many1 $ noneOf "\n"
    wd <- getState
    return $ Dir (wd ++ name)

file :: GenParser Char String File
file = do
    s <- number
    char ' '
    name <- many1 $ noneOf "\n"
    wd <- getState
    return $ File (wd ++ name) s
