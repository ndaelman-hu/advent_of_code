{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.List (splitAt)

main :: IO ()
main = print . f $ read <$> "234234234234278"

f :: [Int] -> Selector [Int]
f xs = (\x -> mapFst show (byMax x)) =<< (slosh byMax Selector hs) <*> Selector ts
  where 
    byLast xs = (init xs, last xs)
    (hs, ts) = splitl byLast (return xs)

-- Selector definitions

data Selector a = Selector (String, a) {_selStr :: String, _selVal :: a}

makeLenses ''Selector

instance Functor Selector where
  fmap :: (a -> b) -> Selector a -> Selector b
  fmap f Selector (s, x) = Selector (s, f x)

instance Applicative Selector where
  pure :: a -> Selector a 
  pure x = Selector ("", x)

  (<*>) :: Selector (a -> b) -> Selector a -> Selector b
  Selector (s1, f) <*> Selector (s2, x) = Selector (s1 ++ s2, f x)

instance Monad Selector where
  return = pure

  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  Selector (s1, x) >>= f = Selector (s1 ++ s2, y)
    where Selector (s2, y) = f x

-- extra Selector functions

slosh :: ([a] -> (Int, a)) -> Selector [a] -> Selector [a]
slosh f (Selector (s, xs)) = Selector (s ++ show y, removeAt j xs)
  where (y, j) = f xs

splitl :: ([a] -> ([a], [a])) -> Selector [a] -> (Selector [a], Selector [a])
splitl f Selector (s, xs) = (Selector (s, ls), Selector ("", rs))
  where (ls, rs) = f xs

-- generic list selection

removeAt :: Int -> [a] -> [a]
removeAt i xs = front ++ tail back
  where (front, back) = splitAt i xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
