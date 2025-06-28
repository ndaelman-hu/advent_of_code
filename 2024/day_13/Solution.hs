import Numeric.LinearAlgebra
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  let problems = [
        (m2 [94, 34, 22, 67], vector [8400, 5400]),
        (m2 [26, 66, 67, 21], vector [12748, 12176]),
        (m2 [17, 86, 84, 37], vector [7870, 6450]),
        (m2 [69, 23, 27, 71], vector [18641, 10279])
        ]
      solutions = [weights sol | (a, b) <- problems, det a /= 0, let sol = a <\> b]
  print $ sum solutions

m2 :: [Double] -> Matrix Double
m2 [a,b,c,d] = (2><2) [a,b,c,d]

weights :: Vector Double -> Double
weights v = 3 * (v ! 0) + (v ! 1)

-----------------------------

pv :: Parser (Vector Double)
pv = do
  _ <- string "Prize: X="
  x <- read <$> many1 digit
  _ <- string ", Y="
  y <- read <$> many1 digit
  return $ vector [fromIntegral x, fromIntegral y]
