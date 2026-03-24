import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile rows "input.test.txt"
  case inp of
    Left e -> print e
    Right r -> print . maximum . f $ multElem snd (multElem fst r)

f :: [(Int, Int)] -> [(Int, ((Int, Int), (Int, Int)))]
f ts = zip rs [compCorners x y | x<-xs, y<-ys]
  where
    (xs, ys) = unzip ts
    rs = zipWith (*) (allDist xs) (allDist ys)

compCorners :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
compCorners (x1, x2) (y1, y2) = ((x1, y2), (y1, x2))

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
