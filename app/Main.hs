import Common (getInput)
import qualified Day1

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
runDay x = putStrLn $ printf "No solution for day '%s' exists" x

printDay :: Show a => IO String -> (IO String -> IO a) -> (IO String -> IO a) -> IO ()
printDay input solve1 solve2 = do
    putStrLn "Part 1:"
    solve1 input >>= print
    putStrLn "Part 2:"
    solve2 input >>= print
