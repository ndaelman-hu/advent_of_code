import Data.Char (digitToInt)
import Data.List
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = let input = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
  in case parse (many row) "" input of
    Left err -> putStrLn $ "Error: " ++ show err
    Right res -> putStrLn . show . bucketCount categorize  -- TODO: add trajectory counter
      $ fmap (++) hrzConn <*> vrtConn $ res

-- counting connectors

type BucketCounter = V.Vector Int
numBuckets = 9 :: Int

zeros :: BucketCounter
zeros = V.fromList $ replicate numBuckets 0

eyes :: Int -> BucketCounter
eyes i = zeros V.// [(i, 1)]

addV :: BucketCounter -> BucketCounter -> BucketCounter
addV = (V.fromList .) . zipWith (+) `on` V.toList

bucketCount ::(a -> BucketCounter) -> [a] -> BucketCounter
bucketCount categorize xs = foldl' (addV) zeros (map categorize xs)

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
genConn = zip <*> drop 1

-- parsing

row :: Parser [Int]
row = many1 (digitToInt <$> digit) <* optional endOfLine
