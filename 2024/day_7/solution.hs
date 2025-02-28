import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = let b = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20\n"
  in case parse body "" b of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> putStrLn . show . sum $
      (\(v, is) -> selForSum (deconstruct v is) v) <$> result

-- solution logic
selForSum :: (Int, String) -> Int -> Int
selForSum res v = if fst res == 0 then v else 0

deconstruct :: Int -> [Int] -> (Int, String)
deconstruct v is = foldr (recursionBody) (v, "") is
    
recursionBody :: Int -> (Int, String) -> (Int, String)
recursionBody i res = (fst inter, (snd inter) : (show i) ++ (snd res))
  where inter = decide (fst res) i

decide :: Int -> Int -> (Int, Char)
decide v i =
  let diff = v - i
  in case () of
  _  | diff == 0 -> (diff, '+')
     | v `mod` i == 0 -> (v `div` i, '*')
     | otherwise -> (diff, '+')

-- parsing logic
body :: Parser [(Int, [Int])]
body = many (line <* newline) <* eof

line :: Parser (Int, [Int])
line = (,) <$> digits <* char ':' <*> many (char ' ' *> digits) -- beware: char over space

digits :: Parser Int
digits = read <$> many1 digit
