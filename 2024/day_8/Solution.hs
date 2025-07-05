import Linear.V2
import Data.List (nub)

main :: IO ()
main = do
  let inputs = [('a', V2 3 4), ('a', V2 4 8), ('a', V2 5 5)]
  print . fullFilter inputs $ concat [solSym $ filter (\x -> fst x==k) inputs | k <- nub $ fmap fst inputs] 

fullFilter :: [Node] -> [Node] -> [Node]
fullFilter ogs ns = zip ks $ filter (inBounds (V2 10 10)) . nub . filter (`notElem` fmap snd ogs) $ vs
  where (ks, vs) = unzip ns

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 rx ry) (V2 x y) = x < rx && y < ry 

solSym :: [Node] -> [Node]
solSym ns = zip ks $ concat $ pairAp compAntiNodes vs
  where (ks, vs) = unzip ns

pairAp :: Applicative f => (a -> a -> b) -> f a -> f b
pairAp f x = f <$> x <*> x

compAntiNodes :: V2 Int -> V2 Int -> [V2 Int]
compAntiNodes v w = [v + d, w - d] 
  where d = v - w

type Node = (Char, V2 Int)
