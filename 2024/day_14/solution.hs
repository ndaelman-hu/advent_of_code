import Data.Function
import Data.List
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser)
-- Data.Vector.Sized ?

main :: IO ()
main = let input="p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3"
  in case parse (many1 line <* eof) "" input of
    Left err -> putStrLn . show $ err
    Right res -> print . innerMultQ $ foldl' (addV) zeroQ ((inQ 51 52) . uncurry (propogate 101 103 100) <$> res)

zeroQ = V.replicate 4 0

setOne :: Int -> V.Vector Int
setOne i = zeroQ V.// [(i, 1)] 

innerMultQ :: V.Vector Int -> Int
innerMultQ = V.foldl' (*) 1

inQ :: Int -> Int -> V.Vector Int -> V.Vector Int
inQ mx my v
  | x == mx = zeroQ
  | y == my = zeroQ
  | x < mx && y < my = setOne 0
  | x > mx && y < my = setOne 1
  | x < mx && y > my = setOne 2
  | x > mx && y > my = setOne 3
    where x = v V.! 0; y = v V.! 1
  
propogate :: Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> V.Vector Int
propogate mx my t p v = modC mx my $ addV p $ scalC t v
  
addV :: V.Vector Int -> V.Vector Int -> V.Vector Int
addV = V.zipWith (+)

scalC :: Int -> V.Vector Int -> V.Vector Int
scalC = fmap . (*)

modC :: Int -> Int -> V.Vector Int -> V.Vector Int
modC mx my v = V.fromList $ map (\x -> mod (v V.! (fst x)) (snd x)) (zip [0..] [mx, my])

-- parsing

line :: Parser (V.Vector Int, V.Vector Int) -- e.g. "p=0,4 v=3,-3"
line = (,) <$> (position <* char ' ') <*> (velocity <* optional newline)

position :: Parser (V.Vector Int)
position = V.fromList <$> sequence [read <$> (string "p=" *> many1 digit), read <$> (char ',' *> many1 digit)]

velocity :: Parser (V.Vector Int)
velocity = V.fromList <$> sequence [string "v=" *> negdigs, char ',' *> negdigs]

negdigs :: Parser Int
negdigs = (*) <$> (option 1 (char '-' >> return (-1))) <*> (read <$> many1 digit)
