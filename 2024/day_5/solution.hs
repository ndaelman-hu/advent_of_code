import Data.List
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  updates_c <- filterEmpty <$> parseFromFile (sepEndBy1 updates newline) "updates.txt"
  rules_c <- parseFromFile (sepEndBy1 rule newline) "rules.txt"
  print updates_c
  print rules_c

-- assumes that each integer only appears once
check :: Integer -> Integer -> [Integer] -> Bool
check i j ks =
  case (elemIndex i ks, elemIndex j ks) of
    (Nothing, _) -> True
    (_, Nothing) -> True
    (Just x, Just y) -> x < y

-- unsafe especially in case of even lists
middlePage :: [Integer] -> Integer
middlePage ks = ks !! div 2 (length ks)

filterEmpty :: Either ParseError [[a]] -> Either ParseError [[a]]
filterEmpty (Right cont) = Right $ filter (not . null) cont
filterEmpty (Left err) = Left err

updates :: Parser [Integer]
updates = sepBy digits (char ',')

rule :: Parser (Integer, Integer)
rule = (,) <$> digits <* char '|' <*> digits

digits :: Parser Integer
digits = read <$> many1 digit 
