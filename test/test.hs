import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "UnitTests"
    [ testCase "Equality" $
        1 == 1 @?= True
    , testCase "Inequality" $
        1 == 2 @?= False
    ]
