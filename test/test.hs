import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

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
    , day3
    , day4
    , day5
    , day6
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

day3 :: TestTree
day3 = testGroup "Day 3"
    [ testCase "Part 1" $ getTestFile "Day3" >>= (@?= 157) . Day3.solve1
    , testCase "Part 2" $ getTestFile "Day3" >>= (@?= 70) . Day3.solve2
    ]

day4 :: TestTree
day4 = testGroup "Day 4"
    [ testCase "Part 1" $ getTestFile "Day4" >>= (@?= 2) . Day4.solve1
    , testCase "Part 2" $ getTestFile "Day4" >>= (@?= 4) . Day4.solve2
    ]

day5 :: TestTree
day5 = testGroup "Day 5"
    [ testCase "Part 1" $ getTestFile "Day5" >>= (@?= "CMZ") . Day5.solve1
    , testCase "Part 2" $ getTestFile "Day5" >>= (@?= "MCD") . Day5.solve2
    ]

day6 :: TestTree
day6 = testGroup "Day 6"
    [ testCase "Part 1 (1)" $ getTestFile "Day6_1" >>= (@?= 7) . Day6.solve1
    , testCase "Part 1 (2)" $ getTestFile "Day6_2" >>= (@?= 5) . Day6.solve1
    , testCase "Part 1 (3)" $ getTestFile "Day6_3" >>= (@?= 6) . Day6.solve1
    , testCase "Part 1 (4)" $ getTestFile "Day6_4" >>= (@?= 10) . Day6.solve1
    , testCase "Part 1 (5)" $ getTestFile "Day6_5" >>= (@?= 11) . Day6.solve1
    , testCase "Part 2 (1)" $ getTestFile "Day6_1" >>= (@?= 19) . Day6.solve2
    , testCase "Part 2 (2)" $ getTestFile "Day6_2" >>= (@?= 23) . Day6.solve2
    , testCase "Part 2 (3)" $ getTestFile "Day6_3" >>= (@?= 23) . Day6.solve2
    , testCase "Part 2 (4)" $ getTestFile "Day6_4" >>= (@?= 29) . Day6.solve2
    , testCase "Part 2 (5)" $ getTestFile "Day6_5" >>= (@?= 26) . Day6.solve2
    ]
