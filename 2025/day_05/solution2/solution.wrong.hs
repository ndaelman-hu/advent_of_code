import Data.Bifunctor (bimap) 
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parser "input.txt"
  case inp of
    Left e -> print e
    Right (ranges, xs) -> print . length $ f ranges xs

f :: [Range] -> [Int]
f ranges = foldl 

newtype Range = Range (Int, Int)
instance Semigroup Range where
  Range a <> Range b = Range (bimap (min (fst a)) (max (snd a)) b)

digits :: Parser Int
digits = read <$> many digit

pRange :: Parser (Int, Int)
pRange = do
  l <- digits
  char '-'
  r <- digits
  return (l, r)

parser :: Parser ([Range], [Int])
parser = do
  l <- endBy pRange newline
  newline
  r <- endBy digits newline
  eof
  return (Range <$> l, r)
