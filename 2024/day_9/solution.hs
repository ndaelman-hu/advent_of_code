import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = case parse diskMap "" ("2333133121414131402" ++ "0") of -- append 0?
  Left err -> putStrLn $ "Error: " ++ show err
  Right res -> putStrLn . show
    $ sum . zipWith (*) [0..]
    $ take (length $ filter (/=Nothing) dm)
    $ switchDec dm (reverse dm)
      where dm = uncurry genDiskMap' (splitDiskMap res)

-- solution
switchDec :: [Maybe Int] -> [Maybe Int] -> [Int]
switchDec norm rev = case (norm, rev) of
  ([], _) -> []
  (_, []) -> []
  (Just n:ns, _) -> (n:switchDec ns rev)
  (Nothing:ns, Just r:rs) -> (r:switchDec ns rs)
  (_, Nothing:rs) -> switchDec norm rs

genDiskMap' :: [Int] -> [Int] -> [Maybe Int]
genDiskMap' fs es = concat $ zipWith3 genDiskMap [0..] fs es

genDiskMap :: Int -> Int -> Int -> [Maybe Int]
genDiskMap ide file empty = take file (repeat (Just ide)) ++ take empty (repeat Nothing)

splitDiskMap :: [Int] -> ([Int], [Int])
splitDiskMap xs =
  let indexed = zip [0..] xs
  in ([x | (i, x) <- indexed, even i], 
      [x | (i, x) <- indexed, odd i])

-- parsing
diskMap :: Parser [Int]
diskMap = many (digitToInt <$> digit)
