import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile ranges "input.test.txt"
  case inp of
    Left e -> print e
    Right r -> print $ f $ t2l =<< r
    -- Right r -> print . sum $ f $ t2l =<< r

f :: [Int] -> [Int]
f x = read . snd <$> filter (\(n, s) -> isOfNLength n s && isValidNId n s) y
  where y = [(i, show x') | i <- [2..maximum (length . show <$> x)], x' <- x]

isOfNLength :: Int -> String -> Bool
isOfNLength n s = mod (length s) n == 0

isValidNId :: Int -> String -> Bool
isValidNId n s = all (== head xs) (tail xs) 
  where
    i = length s `div` n -- is ensured by isOfNLength
    xs = [take i (drop (k*i) s) | k <- [0..n-1]]

t2l :: (Int, Int) -> [Int]
t2l (m, n) = [m..n]

-- parser

range :: Parser (Int, Int)
range = do
  m <- read <$> many1 digit
  char '-'
  n <- read <$> many1 digit
  return (m, n)

ranges :: Parser [(Int, Int)]
ranges = sepBy range (char ',')
