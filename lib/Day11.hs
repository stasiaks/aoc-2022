module Day11 (solve1, solve2) where

import Common.Parser (number)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import Data.List (sort)
import Text.Parsec (modifyState)

type Item = Int
type Monkey = ([Item], Item -> Item, Item -> Int, Int)

solve1 :: String -> Int
solve1 s = monkeyBusiness $ foldl (\a b -> b a) m $ replicate 20 (processRound getBored)
    where (_, m) = parseInput s
solve2 :: String -> Int
solve2 s = monkeyBusiness $ foldl (\a b -> b a) m $ replicate 10000 (processRound c)
    where (xs, m) = parseInput s
          c = copingStrategy (product xs)

monkeyBusiness :: Map Int Monkey -> Int
monkeyBusiness m = product $ take 2 $ reverse $ sort $ map ((\(_, _, _, x) -> x) . snd) $ Map.toList m

processRound :: (Monkey -> Monkey) -> Map Int Monkey -> Map Int Monkey
processRound c m = foldl (\a b -> b a) m fs
    where fs = [ processOneMonkey c n | n <- [0..(length m)] ]

processOneMonkey :: (Monkey -> Monkey) -> Int -> Map Int Monkey -> Map Int Monkey
processOneMonkey c n m = maybe m f a
    where a = Map.lookup n m
          im = Map.update (Just . inspectAll) n m
          f x = removeItems n $ replaceItems im $ (receives . throws . c . inspectAll) x

inspectAll :: Monkey -> Monkey
inspectAll (xs, f1, f2, i) = (map f1 xs, f1, f2, i + length xs)

getBored :: Monkey -> Monkey
getBored (xs, f1, f2, i) = (map (`div` 3) xs, f1, f2, i)

copingStrategy :: Int -> Monkey -> Monkey
copingStrategy a (xs, f1, f2, i) = (map (`mod` a) xs, f1, f2, i)

throws :: Monkey -> [(Item, Int)]
throws (xs, f1, f2, _) = map (\x -> (x, f2 x)) xs

receives :: [(Item, Int)] -> Map Int [Item]
receives xs = f xs Map.empty
    where f [] m = m
          f ((a, b) : ys) m = f ys $ Map.alter (insertOrUpdateList a) b m

replaceItems :: Map Int Monkey -> Map Int [Item] -> Map Int Monkey
replaceItems = myUnionWithKey (\_ (x, f1, f2, i) y -> (x ++ y, f1, f2, i))

removeItems :: Int ->  Map Int Monkey -> Map Int Monkey
removeItems = Map.update (\(_, f1, f2, i) -> Just ([], f1, f2, i))

insertOrUpdateList :: Item -> Maybe [Item] -> Maybe [Item]
insertOrUpdateList a Nothing = Just [a]
insertOrUpdateList a (Just b) = Just (a : b)

myUnionWithKey f = Map.mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id (const Map.empty)

gcd' [] = 1
gcd' [x] = x
gcd' (x:xs) = gcd x (gcd' xs)

-- PARSER --

parseInput :: String -> ([Int], Map Int Monkey)
parseInput s = fromRight ([], Map.empty) $ runParser input [] [] s

input :: GenParser Char [Int] ([Int], Map Int Monkey)
input = do
    result <- sepBy monkey newline
    ds <- getState
    return (ds, Map.fromList result)

monkey :: GenParser Char [Int] (Int, Monkey)
monkey = do
    many1 $ noneOf numChars
    n <- number
    many1 $ noneOf numChars
    items <- sepBy number $ string ", "
    op <- operation
    many1 $ noneOf numChars
    d <- number
    modifyState (d :)
    many1 $ noneOf numChars
    tm <- number
    many1 $ noneOf numChars
    fm <- number
    newline
    return (n, (items, op, \x -> if x `mod` d == 0 then tm else fm, 0))

operation :: GenParser Char st (Item -> Item)
operation = do
    many space
    string "Operation: new = "
    op1 <- operand
    optional space
    opt <- operator
    optional space
    op2 <- operand
    return $ \x -> opt (op1 x) (op2 x)

operator :: GenParser Char st (Item -> Item -> Item)
operator =
    do   char '*' >> return (*)
    <|> (char '-' >> return (-))
    <|> (char '+' >> return (+))
    <|> (char '/' >> return div)

operand :: GenParser Char st (Item -> Item)
operand = do
    oldOperand <|> intLiteral

oldOperand :: GenParser Char st (Item -> Item)
oldOperand = do
    string "old"
    return id

intLiteral :: GenParser Char st (Item -> Item)
intLiteral = do
    const <$> number

numChars = "+-0123456789"

