module Common (getInput, splitOn) where
import Text.Printf (printf)

getInput :: Int -> IO String
getInput n =
    readFile $ printf "inputs/Day%d" n

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn p xs = f xs []
    where f [] acc = [acc]
          f (y : ys) acc = if p == y
                           then acc : f ys []
                           else f ys $ acc ++ [y]

