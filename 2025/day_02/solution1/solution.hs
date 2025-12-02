import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile ranges "input.txt"
  case inp of
    Left e -> print e
    Right r -> print . sum $ f $ join $ fmap t2l r


f :: [Int] -> [Int]
f x = read <$> (filter (\x -> is_even_length x && is_valid_id x) $ fmap show x)

is_even_length :: String -> Bool
is_even_length xs = mod (length xs) 2 == 0 

is_valid_id :: String -> Bool
is_valid_id xs = take j (drop 0 xs) == take j (drop j xs)
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
