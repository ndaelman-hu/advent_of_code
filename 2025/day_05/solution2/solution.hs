import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parser "input.test.txt"
  case inp of
    Left e -> print e
    Right (ranges, xs) -> print . length $ f ranges xs

f :: [(Int, Int)] -> [Int]
f ranges = foldl 

mergeRanges :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
mergeRanges (la, lb) (ra, rb)
  | lconsistent && rconsistent && lb < ra = [(la, lb), (ra, rb)]
  | lconsistent && rconsistent && la > rb = [(ra, rb), (la, lb)]
  | lconsistent && rconsistent && lb >= ra && la < ra = [(la, rb)]
  | lconsistent && rconsistent && la >= ra && lb < rb = [(ra, rb)]
  | lconsistent && rconsistent && lb >= rb && la >= ra = [(ra, lb)]
  | lconsistent && rconsistent && lb >= rb && la < ra = [(la, lb)]
  | otherwise = error "Internal ranges unordered"
  where
    lconsistent = la < lb
    rconsistent = ra < rb

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
