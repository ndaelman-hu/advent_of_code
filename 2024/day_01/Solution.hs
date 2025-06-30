import Data.List (sort)
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  let filename = "questions.txt"
  content <- readFile filename
  case parse (endBy pline newline) filename content of
    Left e -> print e
    Right res -> print $ uncurry solution $ unzip res

solution :: [Int] -> [Int] -> Int
solution listl listr = sum $ abs <$> zipWith (-) (sort listl) (sort listr)

pline :: Parser (Int, Int)
pline = do
  ld <- read <$> many1 digit
  _  <- many1 space
  rd <- read <$> many1 digit
  return (ld, rd)
