import Common (getInput)

import Data.List

main :: IO ()
main = do
    input <- getInput 1
    print $ solve1 $ parse input
    print $ solve2 $ parse input

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn p xs = f xs []
    where f [] acc = [acc]
          f (y : ys) acc = if p == y
                           then acc : f ys []
                           else f ys $ acc ++ [y]

parse :: String -> [[Int]]
parse x = (map . map) read $ splitOn "" $ lines x

solve1 :: [[Int]] -> Int
solve1 xs = maximum $ map sum xs

solve2 :: [[Int]] -> Int
solve2 xs = sum $ take 3 $ (reverse . sort) $ map sum xs
