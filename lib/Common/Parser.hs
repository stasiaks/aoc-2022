module Common.Parser (number) where

import Text.ParserCombinators.Parsec

number :: GenParser Char st Int
number = do
    result <- many1 digit
    return $ read result
