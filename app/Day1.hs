module Main where

import qualified Common (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Common.someFunc
