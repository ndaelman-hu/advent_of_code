import Numeric.LinearAlgebra
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  let problems = [
        (m2 [94, 22, 34, 67], vector [8400, 5400]  ),
        (m2 [26, 67, 66, 21], vector [12748, 12176]),
        (m2 [17, 84, 86, 37], vector [7870, 6450]  ),
        (m2 [69, 27, 23, 71], vector [18641, 10279])
        ]
      solutions = [sol | (a, b) <- problems, det a /= 0, let sol = a <\> b, isClose 1e-5 b (a #> sol)]
  print solutions

m2 :: [Double] -> Matrix Double
m2 [a,b,c,d] = (2><2) [a,b,c,d]

weights :: Vector Double -> Double
weights v = 3 * (v ! 0) + (v ! 1)

isClose :: Double -> Vector Double -> Vector Double -> Bool
isClose eps v1 v2 = norm_2 (v1 - v2) < eps

-----------------------------

pv :: Parser (Vector Double)
pv = do
  _ <- string "Prize: X="
  x <- read <$> many1 digit
  _ <- string ", Y="
  y <- read <$> many1 digit
  return $ vector [fromIntegral x, fromIntegral y]
