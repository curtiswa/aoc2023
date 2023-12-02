import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec (parse, try)
import Text.Parsec.String
import Text.Parsec.Char 
import Text.Parsec.Combinator
import Control.Applicative


main = do
  f <- readFile "part2-input.txt"
  putStrLn $ show $ sum $ map extractNumber $ lines f


extractNumber :: String -> Int
extractNumber str = firstDigit * 10 + secondDigit
  where

  firstDigit = parseLine firstNum str
  secondDigit = parseLine reverseNum $ reverse str

  firstNum :: Parser Int
  firstNum = num <|> textualNum <|> (anyChar >> firstNum)

  num :: Parser Int
  num = digitToInt <$> digit

  textualNum :: Parser Int
  textualNum = mapping numbers

  reverseNum :: Parser Int
  reverseNum = num <|> reverseTextualNum <|> (anyChar >> reverseNum)

  reverseTextualNum :: Parser Int
  reverseTextualNum = mapping [(reverse str, n) | (str, n) <- numbers]

  mapping :: [(String, Int)] -> Parser Int
  mapping [(str, n)] = try (string str >> return n)
  mapping ((str, n):xs) = try (string str >> return n) <|> mapping xs

  parseLine :: Parser Int -> String -> Int
  parseLine parser input = case (parse parser "" input) of
    Right result -> result
    Left e -> error $ show e

  numbers :: [(String, Int)]
  numbers = [("zero", 0),
              ("one", 1),
              ("two", 2),
              ("three", 3),
              ("four", 4),
              ("five", 5),
              ("six", 6),
              ("seven", 7),
              ("eight", 8),
              ("nine", 9)]
