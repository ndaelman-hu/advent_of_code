import Linear.V2
import Data.List (nub)

main :: IO ()
main = do
  let input = [V2 3 4, V2 4 8, V2 5 5]
  print $ filter (inBounds $ V2 10 10) . nub . filter (`notElem` input) . foldr (++) [] $ pairAp compAntiNodes input 

pairAp :: Applicative f => (a -> a -> b) -> f a -> f b
pairAp f x = f <$> x <*> x

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 rx ry) (V2 x y) = if x < rx && y < ry then True else False 

compAntiNodes :: V2 Int -> V2 Int -> [V2 Int]
compAntiNodes v w = [v + d, w - d] 
  where d = v - w
