import Numeric.LinearAlgebra
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  let filename = "questions.txt"
  content <- readFile filename
  case parse (many pvs) filename content of
    Left e -> print e
    Right res -> print . sum . fmap weights $ solution res

solution :: [(Matrix Double, Vector Double)] -> [Vector Double]
solution res = [sol |
  (a, b) <- res,
  det a /= 0,
  let sol = roundVector $ a <\> b,
  all (<= 100) $ toList sol,
  a #> sol == b
  ]

weights :: Vector Double -> Double
weights v = 3 * (v ! 0) + (v ! 1)

m2 :: [Double] -> Matrix Double
m2 = 2><2

-----------------------------

pv :: Parser Double
pv = do
  _ <- manyTill anyChar (oneOf "+=")
  digits <- many1 digit
  return $ fromIntegral (read digits :: Int)

pvs :: Parser (Matrix Double, Vector Double)
pvs = do
  r11 <- pv
  r21 <- pv
  _ <- newline
  r12 <- pv
  r22 <- pv
  _ <- newline
  s1  <- pv
  s2  <- pv
  _ <- skipMany newline
  return (m2 [r11, r12, r21, r22], vector [s1, s2])
