import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile ranges "input.txt"
  case inp of
    Left e -> print e
    Right r -> print . sum $ f $ t2l =<< r


f :: [Int] -> [Int]
f x = read <$> (filter (\x -> isEvenLength x && isValidId x) $ fmap show x)

isEvenLength :: String -> Bool
isEvenLength xs = even (length xs) 

isValidId :: String -> Bool
isValidId xs = take j xs == take j (drop j xs)
  where
    i = length xs
    j = i `div` 2

t2l :: (Int, Int) -> [Int]
t2l (m, n) = [m..n]

-- parser

range :: Parser (Int, Int)
range = do
  m <- read <$> many1 digit
  char '-'
  n <- read <$> many1 digit
  return (m, n)

ranges :: Parser [(Int, Int)]
ranges = sepBy range (char ',')
