import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile rows "input.txt"
  case inp of
    Left e -> print e
    Right r -> print . maximum $ f r

f :: [(Int, Int)] -> [Int]
f ts = zipWith (*) (allDist xs) (allDist ys)
  where (xs, ys) = unzip ts

allDist :: [Int] -> [Int]
allDist xs = [dist x|x<-xs] <*> xs

dist :: Int -> Int -> Int
dist x y = abs (x-y) + 1

row :: Parser (Int, Int)
row = do
  x <- read <$> many digit
  char ','
  y <- read <$> many digit
  return (x, y)

rows :: Parser [(Int, Int)]
rows = endBy row newline
