import Solution
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  rules_c <- parseFromFile (sepEndBy1 rule newline) ri 
  updates_c <- filterEmpty <$> parseFromFile (sepEndBy1 updates newline) ui 
  case (rules_c, updates_c) of
    (Right rules_d, Right updates_d) -> print $ logic rules_d updates_d
    (Left _, Left _) -> error $ "Invalid input formats in " ++ show ri ++ " and " ++ show ui
    (Left _, _) -> error $ "Invalid input format in " ++ show ri
    (_, Left _) -> error $ "Invalid input format in " ++ show ui
  where ri = "rules.txt"; ui = "updates.txt"

filterEmpty :: Either ParseError [[a]] -> Either ParseError [[a]]
filterEmpty (Right cont) = Right $ filter (not . null) cont
filterEmpty (Left err) = Left err

-- parsing

updates :: Parser [Integer]
updates = sepBy digits (char ',')

rule :: Parser (Integer, Integer)
rule = (,) <$> digits <* char '|' <*> digits

digits :: Parser Integer
digits = read <$> many1 digit