import Linear.V2
import Data.List (nub)

main :: IO ()
main = do
  let inputs = [('a', V2 3 4), ('a', V2 4 8), ('a', V2 5 5)]
  print . fullFilter inputs $ solSymb inputs

fullFilter :: [Node] -> [Node] -> [Node]
fullFilter ogs ns = zip ks $ filter (inBounds (V2 10 10)) . nub . filter (`notElem` fmap snd ogs) $ vs
  where (ks, vs) = unzip ns

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 rx ry) (V2 x y) = x < rx && y < ry 

solSymb :: [Node] -> [Node]
solSymb = concat . pairAp compAntiNodes

pairAp :: Applicative f => (a -> a -> b) -> f a -> f b
pairAp f x = f <$> x <*> x

compAntiNodes :: Node -> Node -> [Node]
compAntiNodes (k, v) (l, w) =
  if k == l
    then [(k, v + d), (l, w - d)]
    else []
  where d = v - w

type Node = (Char, V2 Int)
