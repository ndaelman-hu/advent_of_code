import Data.List (sortBy)
import Data.Ord (comparing, Down(Down))
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile rows "input.test.txt"
  case inp of
    Left e -> print e
    Right r -> print $ fmap fst . sortBy (comparing snd) $ unIVal $ g 12 (indexList (fmap read r)) (indexList [])

g :: Int -> IVal Int -> IVal Int -> IVal Int
g lim start end = if length (unIVal end) > lim
  then let fInt = f start
           newStart = IVal $ tail . unIVal $ fInt
           newEnd = IVal $ head (unIVal fInt) : unIVal end
        in g lim newStart newEnd
  else end

f :: Ord a => IVal a -> IVal a
f (IVal xs) = IVal $ filter (\x -> snd x >= firstI ys) ys
  where IVal ys = highestFirst . IVal . sortBy (comparing (Down . fst)) $ xs -- sort desc 

highestFirst :: IVal a -> IVal a -- type sgn can be relaxed
highestFirst (IVal xs) = if firstI xs == length xs then IVal (swap12 xs) else IVal xs

swap12 :: [a] -> [a] -- swap first 2 list elements
swap12 (x:y:zs) = y:x:zs  -- handles also if zs == []
swap12 xs = xs  -- handles [], [x]

firstI :: [(a, Int)] -> Int 
firstI = snd . head

unIVal :: IVal a -> [(a, Int)]
unIVal (IVal xs) = xs

indexList :: [a] -> IVal a
indexList xs = IVal $ zip xs [1..]

newtype IVal a = IVal [(a,Int)]
  deriving (Eq,Ord,Show)

-- parsing

rows :: Parser [String]
rows = init <$> sepBy (many digit) newline
