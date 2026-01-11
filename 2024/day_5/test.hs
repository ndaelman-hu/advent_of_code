import Test.QuickCheck
import Solution (check, middlePage)
import Data.List (nub)

main :: IO ()
main = do
  quickCheck reverseCheck 

reverseCheck :: Integer -> Integer -> [Integer] -> Property
reverseCheck i j ks = 
  let uniqueKs = nub ks
  in length uniqueKs > 1 && i `elem` uniqueKs && j `elem` uniqueKs && i /= j ==> 
     check i j uniqueKs /= check i j (reverse uniqueKs) 
