module Solution 
  ( check
  , middlePage
  , logic
  , checkAll
  ) where

import Data.List
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)


logic :: [(Integer, Integer)] -> [[Integer]] -> Integer 
logic rules updates = sum $ map middlePage (checkAll rules updates)

checkAll :: [(Integer, Integer)] -> [[Integer]] -> [[Integer]]
checkAll rules = filter (\update -> all (\rule -> uncurry check rule update) rules) 

check :: Integer -> Integer -> [Integer] -> Bool
check _ _ [] = error "check: cannot check rules of empty list"
check i j ks =
  if length ks == Set.size (Set.fromList ks)
  then case (elemIndex i ks, elemIndex j ks) of
      (Just x, Just y) -> x < y
      _ -> True
  else error $ "check: requires different pages in " ++ show ks

middlePage :: [Integer] -> Integer
middlePage [] = error "middlePage: cannot find middle of empty list"
middlePage ks 
  | even l = error $ "middlePage: updates " ++ show ks ++ " has even length"
  | otherwise = ks !! div l 2
    where l = length ks

 
