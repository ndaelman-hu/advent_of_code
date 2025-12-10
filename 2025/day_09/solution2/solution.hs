import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile rows "input.test.txt"
  case inp of
    Left e -> print e
    Right r -> print $ filter (>0) [filtByLink x y c|x<-r, y<-r, c<-r]

filtByLink :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
filtByLink x y c
  | fst x == fst c && snd y >= snd c = form
  | snd y == snd c && fst x >= fst c = form
  | otherwise = 0
  where form = dist (fst x) (fst y) * dist (snd x) (snd y)

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
