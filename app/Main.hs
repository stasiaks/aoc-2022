import Common (getInput)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "No day provided"
      n:_ -> runDay n

runDay :: String -> IO ()
runDay ('D' : 'a' : 'y' : n) = runDay n
runDay ('0' : n) = runDay n
runDay "1" = printDay (getInput 1) Day1.solve1 Day1.solve2
runDay "2" = printDay (getInput 2) Day2.solve1 Day2.solve2
runDay "3" = printDay (getInput 3) Day3.solve1 Day3.solve2
runDay "4" = printDay (getInput 4) Day4.solve1 Day4.solve2
runDay "5" = printDay (getInput 5) Day5.solve1 Day5.solve2
runDay "6" = printDay (getInput 6) Day6.solve1 Day6.solve2
runDay x = putStrLn $ printf "No solution for day '%s' exists" x

printDay :: Show a => IO String -> (String -> a) -> (String -> a) -> IO ()
printDay io solve1 solve2 = do
    input <- io
    putStrLn "Part 1:"
    print $ solve1 input
    putStrLn "Part 2:"
    print $ solve2 input
