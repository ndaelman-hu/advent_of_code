import Linear.Vector
import Linear.V3
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = putStrLn . show . sum
  $ (\x -> if gtV (V3 12 13 14) then fst x else 0) . (compactV snd)
  <$> parse game "" b
  where b = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

-- solution
gtV :: V3 -> V3 -> Bool
gtV = all (uncurry (>) $ toList) zip -- first var: ref

compactV :: [V3] -> V3
compactV = foldl' (^+^) (V3 0 0 0)

-- parser
game :: Parser (Int, [V3])
game = (,) <$> (read $ string "Game " *> digits <* char ':') <*> session

session :: Parser [V3]
session = many1 (char ' ' *> try red <|> try green <|> blue <* char ',' ) <* choice [char ';', newline]

red :: Parser V3
red = V3 (char ' ' *> digits <* string " red") 0 0

green :: Parser V3
green = V3 0 (char ' ' *> digits <* string " green") 0

blue :: Parser V3
blue = V3 0 0 (char ' ' *> digits <* string " blue")

digits :: Parser Int
digits = read $ many1 digit
