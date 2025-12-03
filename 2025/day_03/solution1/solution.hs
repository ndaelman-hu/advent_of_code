{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.List (splitAt)

main :: IO ()
main = print . f $ read <$> "234234234234278"

f :: [Int] -> Selector [Int]
f xs = (\x -> mapFst show (byMax x)) =<< (slosh byMax (Selector hs)) <*> Selector ts
  where 
    byLast xs = (init xs, last xs)
    (hs, ts) = splitl byLast (return xs)

-- Selector definitions

data Selector a = Selector {_selStr :: String, _selVal :: a}

makeLenses ''Selector

instance Functor Selector where
  fmap :: (a -> b) -> Selector a -> Selector b
  fmap f sel = sel & selVal %~ f

instance Applicative Selector where
  pure :: a -> Selector a 
  pure = Selector ""

  (<*>) :: Selector (a -> b) -> Selector a -> Selector b
  sel1 <*> sel2 = Selector
    (sel1 ^. selStr ++ sel2 ^. selStr)
    (sel1 ^. selVal $ sel2 ^. selVal)

instance Monad Selector where
  return = pure

  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  sel >>= f = Selector (sel ^. selStr ++ selh ^. selStr) (selh ^. selVal)
      where selh = f $ sel ^. selVal 

-- extra Selector functions

slosh :: ([a] -> (Int, a)) -> Selector [a] -> Selector [a]
slosh f sel = Selector (s ++ show y) (removeAt j xs)
  where
    xs = sel ^. selVal
    (y, j) = f xs

splitl :: ([a] -> ([a], [a])) -> Selector [a] -> (Selector [a], Selector [a])
splitl f sel = (Selector (sel ^. selStr, ls), Selector ("", rs))
  where
    xs = sel ^. selVal
    (ls, rs) = f xs

-- generic list selection

removeAt :: Int -> [a] -> [a]
removeAt i xs = front ++ tail back
  where (front, back) = splitAt i xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
