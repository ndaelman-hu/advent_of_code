import Linear.V2
import Data.List (nub)

main :: IO ()
main = do
  let input = [V2 3 4, V2 4 8, V2 5 5]
  print $ nub . filter (`notElem` input) . foldr (++) [] $ pairAp compAntiNodes input 

pairAp :: Applicative f => (a -> a -> b) -> f a -> f b
pairAp f x = f <$> x <*> x

compAntiNodes :: V2 Int -> V2 Int -> [V2 Int]
compAntiNodes v w = [v + d, w - d] 
  where d = v - w
