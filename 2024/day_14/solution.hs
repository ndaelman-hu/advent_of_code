import Data.Function
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = let input="p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3"
  in case parse (many1 line <* eof) "" input of
    Left err -> putStrLn . show $ err
    Right res -> print . innerMultQ $ foldl' (addQ) zeroQ ((inQ 51 52) . uncurry (propogate 101 103 100) <$> res)

newtype Quadr = Quadr (Int, Int, Int, Int)

zeroQ = Quadr (0, 0, 0, 0)

innerMultQ :: Quadr -> Int
innerMultQ (Quadr (x, y, z, u)) = x * y * z * u

inQ :: Int -> Int -> Coord -> Quadr
inQ mx my (Coord (x, y))
  | x == mx = zeroQ
  | y == my = zeroQ
  | x < mx && y < my = Quadr (1, 0, 0, 0)
  | x > mx && y < my = Quadr (0, 1, 0, 0)
  | x < mx && y > my = Quadr (0, 0, 1, 0)
  | x > mx && y > my = Quadr (0, 0, 0, 1)

addQ :: Quadr -> Quadr -> Quadr
addQ (Quadr (x1, y1, z1, u1)) (Quadr (x2, y2, z2, u2)) = Quadr (x1 + x2, y1 + y2, z1 + z2, u1 + u2)

newtype Coord = Coord (Int, Int)
  deriving (Show, Read)
  
propogate :: Int -> Int -> Int -> Coord -> Coord -> Coord
propogate mx my t p v = modC mx my $ addC p $ scalC t v
  
addC :: Coord -> Coord -> Coord
addC (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)

scalC :: Int -> Coord -> Coord
scalC i (Coord (x, y)) = Coord (i * x, i * y)

modC :: Int -> Int -> Coord -> Coord
modC mx my (Coord (x, y)) = Coord (x `mod` mx, y `mod` my)

-- parsing

line :: Parser (Coord, Coord) -- e.g. "p=0,4 v=3,-3"
line = (,) <$> (position <* char ' ') <*> (velocity <* optional newline)

position :: Parser Coord
position = Coord <$> ((,) <$> (read <$> (string "p=" *> many1 digit)) <*> (read <$> (char ',' *> many1 digit)))

velocity :: Parser Coord
velocity = Coord <$> ((,) <$> (string "v=" *> negdigs) <*> (char ',' *> negdigs))

negdigs :: Parser Int
negdigs = (*) <$> (option 1 (char '-' >> return (-1))) <*> (read <$> many1 digit)
