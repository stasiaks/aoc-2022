{-# LANGUAGE FlexibleInstances #-}

import Common (getInput)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11

import System.Environment (getArgs)
import Text.Printf (printf)

class Print a where
    printStrAware :: a -> IO ()

instance Print Int where
    printStrAware = print

instance Print [Char] where
    printStrAware = putStrLn

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
runDay "7" = printDay (getInput 7) Day7.solve1 Day7.solve2
runDay "8" = printDay (getInput 8) Day8.solve1 Day8.solve2
runDay "9" = printDay (getInput 9) Day9.solve1 Day9.solve2
runDay "10" = printDay (getInput 10) Day10.solve1 Day10.solve2
runDay "11" = printDay (getInput 11) Day11.solve1 Day11.solve2
runDay x = putStrLn $ printf "No solution for day '%s' exists" x

printDay :: Print a => Print b => IO String -> (String -> a) -> (String -> b) -> IO ()
printDay io solve1 solve2 = do
    input <- io
    putStrLn "Part 1:"
    printStrAware $ solve1 input
    putStrLn "Part 2:"
    printStrAware $ solve2 input

