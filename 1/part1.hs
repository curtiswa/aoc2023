import Data.Char
import Data.List
import Data.Maybe

main = do
  f <- readFile "part1-input.txt"
  putStrLn $ show $ sum $ map extractNumber $ lines f


extractNumber :: String -> Int
extractNumber str = firstDigit * 10 + secondDigit
  where
  firstDigit = getFirstDigit str
  secondDigit = getFirstDigit $ reverse $ str

  getFirstDigit s = digitToInt $ fromJust $ find isDigit s
