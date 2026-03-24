{-# LANGUAGE MultiWayIf #-}

import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile dial "input2.txt"
  case inp of
    Left e -> print e
    Right r -> print . f $ r

-- parsing

rotation :: Parser Counter
rotation = do
  sgn <- char 'L' $> (* (-1)) <|> char 'R' $> id
  n <- many1 digit
  return $ Counter (sgn (read n), 0)

dial :: Parser [Counter]
dial = many (rotation <* newline)

-- core logic

newtype Counter = Counter (Integer, Integer)
  deriving Show

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

instance Semigroup Counter where
  Counter (x1, x2) <> Counter (y1, y2) = let
    dialSize = 100
    (y1q, y1r) = y1 `quotRem` dialSize
    smallDial' = x1 + y1r
    (bigDial', smallDial) =  smallDial' `divMod` dialSize
    bigDial = sum $ fmap abs [x2,y2,y1q,bigDial']
    in Counter (smallDial, bigDial)

instance Monoid Counter where
  mempty = Counter (0, 0)

f :: [Counter] -> [Counter]
f = scanl (<>) (Counter (50, 0))
