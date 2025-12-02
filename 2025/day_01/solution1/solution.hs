import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile dial "input.txt"
  case inp of
    Left e -> print e
    Right r -> print . f $ r

f :: [Int] -> Int
f = length . filter (==0) . fmap (`mod` 100) . scanl (+) (-50)

rotation :: Parser Int
rotation = do
  sgn <- char 'L' $> (* (-1)) <|> char 'R' $> id
  n <- many1 digit
  return $ sgn (read n)

dial :: Parser [Int]
dial = many (rotation <* newline)
