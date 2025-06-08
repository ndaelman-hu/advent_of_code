import Data.List
import qualified Data.Set as Set
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

logic :: [(Integer, Integer)] -> [[Integer]] -> Integer 
logic rules updates = sum $ map middlePage (checkAll rules updates)

checkAll :: [(Integer, Integer)] -> [[Integer]] -> [[Integer]]
checkAll rules = filter (\update -> all (\rule -> uncurry check rule update) rules) 

-- assumes that each integer only appears once
check :: Integer -> Integer -> [Integer] -> Bool
check _ _ [] = error "check: cannot check rules of empty list"
check i j ks =
  if length ks == Set.size (Set.fromList ks)
  then case (elemIndex i ks, elemIndex j ks) of
      (Just x, Just y) -> x < y
      _ -> True
  else error $ "check: requires different pages in " ++ show ks

middlePage :: [Integer] -> Integer
middlePage [] = error "middlePage: cannot find middle of empty list"
middlePage ks 
  | even l = error $ "middlePage: updates " ++ show ks ++ " has even length"
  | otherwise = ks !! div l 2
    where l = length ks

filterEmpty :: Either ParseError [[a]] -> Either ParseError [[a]]
filterEmpty (Right cont) = Right $ filter (not . null) cont
filterEmpty (Left err) = Left err

updates :: Parser [Integer]
updates = sepBy digits (char ',')

rule :: Parser (Integer, Integer)
rule = (,) <$> digits <* char '|' <*> digits

digits :: Parser Integer
digits = read <$> many1 digit 
