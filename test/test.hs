import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import qualified Day1
import qualified Day2

main :: IO ()
main = defaultMain tests

getTestFile :: String -> IO String
getTestFile s =
    readFile $ printf "test/inputs/%s" s

tests :: TestTree
tests = testGroup "Tests" [ solutions ]

solutions :: TestTree
solutions = testGroup "Daily solutions"
    [ day1
    , day2
    ]

day1 :: TestTree
day1 = testGroup "Day 1"
    [ testCase "Part 1" $ getTestFile "Day1" >>= (@?= 24000) . Day1.solve1
    , testCase "Part 2" $ getTestFile "Day1" >>= (@?= 45000) . Day1.solve2
    ]

day2 :: TestTree
day2 = testGroup "Day 2"
    [ testCase "Part 1" $ getTestFile "Day2" >>= (@?= 15) . Day2.solve1
    , testCase "Part 2" $ getTestFile "Day2" >>= (@?= 12) . Day2.solve2
    ]
