import Data.List (minimumBy, maximumBy, partition)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parser "input.test.txt"
  case inp of
    Left e -> print e
    Right (ranges, xs) -> print $ f ranges []

f :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
f xs results = 
  if null xs
     then results
     else let 
      (lowest, batch) = pickLowest xs
      (newLowest, nextBatch) = selectRange lowest batch
      in case newLowest of
        Left nL -> f (nL:tail results) nextBatch 
        Right nL -> f (nL:results) nextBatch

selectRange :: (Int, Int) -> [(Int, Int)] -> (Either (Int, Int) (Int, Int), [(Int, Int)]) -- assume 1st argument minimum lower-bound
selectRange (lmin, rmin) xs = 
  if null ys
     then let (minRange, rrest) = pickLowest xs in (Right minRange, rrest) 
     else let (maxRange, _) = pickLowest ys in  (Left (lmin, snd maxRange), rest)
  where (ys, rest) = partition (\x -> fst x >= rmin) xs

-- Note: head, minimum, minimumBy, etc. all throw errors for empty lists

pickLowest :: [(Int, Int)] -> ((Int, Int), [(Int, Int)]) -- pick highest right-boundary among lowest left boundaries
pickLowest ranges = (maximumBy (comparing snd) minVals, rest)
  where
    (minVals, rest) = partition (\x -> fst x == fst minVal) ranges
    minVal = minimumBy (comparing fst) ranges

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
