import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (catMaybes)
import Data.List (foldl')

main :: IO ()
main = do
  let testInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  case parse parseFlexible "" testInput of
    Left err -> putStrLn $ show $ err
    Right res -> putStrLn $ show $ foldl' (+) 0 (runMul <$> res)

parseFlexible :: Parser [Multiplication]
parseFlexible = do
  results <- many $ choice [
    try multiplier >>= \mul -> return (Just mul),
    anyChar >> return Nothing
    ]
  return $ catMaybes results

multiplier :: Parser Multiplication
multiplier = do
  string "mul("
  x <- triple
  char ','
  y <- triple
  char ')'
  return (Mul x y)

triple :: Parser Int
triple = do 
  ds <- try (count 3 digit) <|> try (count 2 digit) <|> count 1 digit
  return $ read ds -- convert comma-separated [Char] to String

data Multiplication = Mul Int Int
  deriving (Show, Read)

runMul :: Multiplication -> Int
runMul (Mul x y) = x * y
