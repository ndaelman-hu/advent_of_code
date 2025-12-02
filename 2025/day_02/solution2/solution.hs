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
f x = read . snd <$> filter (\(n, xs) -> isOfNLength n xs && isValidNId n xs) y
  where y = [(i, show x') | i <- [2..maximum (length . show <$> x)], x' <- x]

isOfNLength :: Int -> String -> Bool
isOfNLength n xs = mod (length xs) n == 0

isValidNId :: Int -> String -> Bool
isValidNId n xs = all (== take j xs) [take (k * (j + 1)) (drop (k * j) xs) | k <- [1..n-1]]
  where
    i = length xs
    j = i `div` n -- is ensured by isOfNLength

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
