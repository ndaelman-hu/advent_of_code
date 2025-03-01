import Control.Monad

main :: IO ()
main = putStrLn . show . length
  $ iterate (\xs -> xs >>= rules) stones !! n
  where n = 25; stones = [125, 17]

rules :: Int -> [Int]
rules b = case b of
  0 -> [1]
  _ | (length . show) b `mod` 2 == 0 -> splitInt b
  otherwise -> [2024 * b]

splitInt :: Int -> [Int]
splitInt b =
  if bn < 2
    then [b]
    else [read $ take bn2 bb, read $ drop bn2 bb]
  where bb = show b; bn = length bb; bn2 = bn `div` 2;
