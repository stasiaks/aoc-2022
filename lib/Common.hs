module Common where
import Text.Printf (printf)

getInput :: Int -> IO String
getInput n =
    readFile $ printf "inputs/Day%d" n
