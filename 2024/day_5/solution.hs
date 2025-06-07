import Data.List
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  rules_c <- parseFromFile (sepEndBy1 rule newline) "rules.txt"
  updates_c <- filterEmpty <$> parseFromFile (sepEndBy1 updates newline) "updates.txt"
  case (rules_c, updates_c) of
    (Right rules_d, Right updates_d) -> print $ logic rules_d updates_d
    _ -> print "Invalid input format"

logic :: [(Integer, Integer)] -> [[Integer]] -> Integer 
logic rules updates = sum $ map middlePage (filter (\update -> all (\rule -> uncurry check rule update) rules) updates)

-- assumes that each integer only appears once
check :: Integer -> Integer -> [Integer] -> Bool
check i j ks =
  case (elemIndex i ks, elemIndex j ks) of
    (Just x, Just y) -> x < y
    _ -> True

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
