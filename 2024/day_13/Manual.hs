solve = (,) <$> ... <*> id $ ...

subst :: Equ 

uppertriag :: Eqs -> Eqs   
uppertriag eqs = 

firstZ :: [Int] -> Maybe Int
firstZ [] = Nothing
firstZ r:rs = if r == 0 then firstZ rs else Just r 

det :: Int -> Int -> Int -> Int -> Int 
det a b c d = a * c - b * d 

type Matrix = [[Int]]
type Eqs = {
  m :: Matrix,
  v :: [Int]
  }
