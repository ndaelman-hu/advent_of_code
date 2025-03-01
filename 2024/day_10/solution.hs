import Data.List
import qualified Numeric.LinearAlgebra as H

main :: IO ()
main = putStrLn . show . bucketCount categorize  -- TODO: add trajectory counter
  $ hrzConn input ++ vrtConn input
    where input = [[0,1,2,3], [1,2,3,4], [8,7,6,5], [9,8,7,6]]

-- counting connectors

type BucketCounter = H.Vector Int
numBuckets = 9 :: Int

zeros :: BucketCounter
zeros = H.fromList $ replicate numBuckets 0

eyes :: Int -> BucketCounter
eyes i = zeros H.#|> [(i, 1)]

bucketCount ::(a -> BucketCounter) -> [a] -> BucketCounter
bucketCount categorize xs = foldl' (H.+.) zeros (map categorize xs)

categorize ::(Int, Int) -> BucketCounter
categorize x =
  if fst x + 1 == snd x
    then eyes $ fst x
    else zeros

-- setting up connectors
vrtConn :: [[a]] -> [(a, a)]
vrtConn = hrzConn . transpose

hrzConn :: [[a]] -> [(a, a)]
hrzConn = concatMap genConn

genConn :: [a] -> [(a, a)]
genConn = zip <*> tail
