import Common (getInput)
import qualified Day1
import qualified Day2

import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "No day provided"
      n:_ -> runDay n

runDay :: String -> IO ()
runDay ('D' : 'a' : 'y' : '0' : n) = runDay n
runDay ('D' : 'a' : 'y' : n) = runDay n
runDay "1" = printDay (getInput 1) Day1.solve1 Day1.solve2
runDay "2" = printDay (getInput 2) Day2.solve1 Day2.solve2
runDay x = putStrLn $ printf "No solution for day '%s' exists" x

printDay :: Show a => IO String -> (String -> a) -> (String -> a) -> IO ()
printDay io solve1 solve2 = do
    input <- io
    putStrLn "Part 1:"
    print $ solve1 input
    putStrLn "Part 2:"
    print $ solve2 input
