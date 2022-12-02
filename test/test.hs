import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import qualified Day1

main :: IO ()
main = defaultMain tests

getTestFile :: String -> IO String
getTestFile s =
    readFile $ printf "test/inputs/%s" s

tests :: TestTree
tests = testGroup "Tests" [ solutions ]

solutions :: TestTree
solutions = testGroup "Daily solutions"
    [ day1 ]

day1 :: TestTree
day1 = testGroup "Day 1"
    [ testCase "Part 1" $ Day1.solve1 (getTestFile "Day1") >>= (@?= 24000)
    , testCase "Part 2" $ Day1.solve2 (getTestFile "Day1") >>= (@?= 45000)
    ]
