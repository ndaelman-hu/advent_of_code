import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile dial "input.txt"
  case inp of
    Left e -> print e
    Right r -> print . f $ r

rotation :: Parser Counter
rotation = do
  sgn <- char 'L' $> (* (-1)) <|> char 'R' $> id
  n <- many1 digit
  return $ Counter (sgn (read n), 0)

dial :: Parser [Counter]
dial = many (rotation <* newline)

-- ━━━ Monoid (T) ━━━
-- Carrier: 
-- Operation: (<>) :  →  → 
-- Identity: mempty : 
--
-- Laws (extend Semigroup):
--   (assoc)   ∀ a b c. (a <> b) <> c ≡ a <> (b <> c)
--   (left-id) ∀ a. mempty <> a ≡ a
--   (right-id)∀ a. a <> mempty ≡ a
--   (mconcat) ∀ xs. mconcat xs ≡ foldr (<>) mempty xs

newtype Counter = Counter (Integer, Integer)
  deriving Show

instance Semigroup Counter where
  Counter (x1, x2) <> Counter (y1, y2) = let
    dialSize = 100
    counter = x1 + y1
    smallDial = counter `mod` dialSize
    bigDial = if smallDial == 0 then 1 else 0
    in Counter (smallDial, x2 + y2 + bigDial)

instance Monoid Counter where
  mempty = Counter (0, 0)

f :: [Counter] -> Counter
f = foldl (<>) (Counter (50, 0)) -- l is needed, as the monoid is not commutative
