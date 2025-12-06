import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parser "input.txt"
  case inp of
    Left e -> print e
    Right (ranges, xs) -> print . length $ f ranges xs

f :: [(Int, Int)] -> [Int] -> [Int]
f ranges xs = concat [if or [checkRange r x | r <- ranges] then [x] else [] | x <- xs]

checkRange :: (Int, Int) -> (Int -> Bool)
checkRange (i, j) x = (i <= x) && (x <= j)

digits :: Parser Int
digits = read <$> many digit

pRange :: Parser (Int, Int)
pRange = do
  l <- digits
  char '-'
  r <- digits
  return (l, r)

parser :: Parser ([(Int, Int)], [Int])
parser = do
  l <- endBy pRange newline
  newline
  r <- endBy digits newline
  eof
  return (l, r)
