module Common.Parser (number) where

import Text.ParserCombinators.Parsec

number :: GenParser Char st Int
number = do
    s <- sign
    result <- many1 digit
    return $ s $ read result

sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)
