import Data.List (sortBy)
import Data.Ord (comparing, Down(Down))

main :: IO ()
main = print . f $ indexList "234234234234278"

f :: IVal a -> [a]
f (IVal xs) = fst <$> take 2 (filter (\x -> snd x >= firstI ys) ys)
  where IVal ys = highestFirst . IVal . sortBy (comparing (Down . snd)) $ xs -- sort desc 

highestFirst :: IVal a -> IVal a -- type sgn can be relaxed
highestFirst (IVal xs) = if firstI xs == length xs then IVal (swap12 xs) else IVal xs

swap12 :: [a] -> [a] -- swap first 2 list elements
swap12 (x:y:zs) = y:x:zs
swap12 xs = xs  -- handles [], [x]

firstI :: [(a, Int)] -> Int 
firstI = snd . head

indexList :: [a] -> IVal a
indexList xs = IVal $ zip xs [1..]

newtype IVal a = IVal [(a,Int)]
  deriving (Eq,Ord)
