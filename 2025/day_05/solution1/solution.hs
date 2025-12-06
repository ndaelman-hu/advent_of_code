import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parser "input.test.txt"
  case inp of
    Left e -> print e
    Right (ranges, xs) -> print . sum $ f ranges xs

f :: [(Int, Int)] -> [Int] -> [Int]
f ranges xs = concat [[x | x `elem` urange] | x <- xs]
  where urange = concatMap buildRange ranges

buildRange :: (Int, Int) -> [Int]
buildRange (i, j) = [i..j]

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
