main :: IO ()
main = putStrLn $ show $ conditions <$> input
  where input = [[7, 6, 4, 2, 1],
                 [1, 2, 7, 8, 9],
                 [9, 7, 6, 2, 1],
                 [1, 3, 2, 4, 5],
                 [8, 6, 4, 4, 1],
                 [1, 3, 6, 7, 9]]

conditions :: [Int] -> Bool
conditions row = (check2 (-1) drow || check2 1 drow) && (check3 $ abs <$> drow)
  where drow = diff row

check2 :: Int -> [Int] -> Bool
check2 sign dds = (replicate (length dds) sign) == (signum <$> dds)

check3 :: [Int] -> Bool
check3 dds = case dds of 
  [] -> True
  (d:ds) 
    | 1 <= d && d <= 3 -> True && check3 ds
    | otherwise -> False

diff :: [Int] -> [Int]
diff row = zipWith (-) (tail row) row
