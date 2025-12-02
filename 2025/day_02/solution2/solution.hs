import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile ranges "input.txt"
  case inp of
    Left e -> print e
    Right r -> print . sum $ f $ t2l =<< r
    -- Right r -> print $ f $ t2l =<< r

f :: [Int] -> [Int]
f x = read <$> filter isValidNId (show <$> x)

isValidNId :: String -> Bool
isValidNId s = 
  let factors = uniqueDividersNoPrime (length s)
      chunks  = fmap (`splitEqChunks` s) factors
  in not (null factors) && any (\xs -> all (== head xs) (tail xs)) chunks

splitEqChunks :: Int -> String -> [String] -- requires all dividers, else produces list with the last element of shorter length
splitEqChunks nchunk s = [take ssize (drop (k*ssize) s) | k <- [0..nchunk-1]]
  where ssize = length s `div` nchunk

uniqueDividersNoPrime :: Int -> [Int]
uniqueDividersNoPrime n = fact n [] -- ref factor collection
  where
    fact 1 xs = xs -- [n-1..1] excludes primes
    fact m xs = fact (m-1) (if n `mod` m == 0 then m:xs else xs)

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
