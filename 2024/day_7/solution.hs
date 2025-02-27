main :: IO ()
main = (putStrLn . show . sum) $
  (\(v, is) -> selForSum (deconstruct v is) v) <$>
  [(190, [10, 19])
  ,(3267, [81, 40, 27])
  ,(83, [17, 5])
  ,(156, [15, 6])
  ,(7290, [6, 8, 6, 15])
  ,(161011, [16, 10, 13])
  ,(192, [17, 8, 14])
  ,(21037, [9, 7, 18, 13])
  ,(292, [11, 6, 16, 20])]
  
selForSum :: (Int, String) -> Int -> Int
selForSum res v = if fst res == 0 then v else 0

deconstruct :: Int -> [Int] -> (Int, String)
deconstruct v is = foldr (recursionBody) (v, "") is
    
recursionBody :: Int -> (Int, String) -> (Int, String)
recursionBody i res = (fst inter, (snd inter) : (show i) ++ (snd res))
  where inter = decide (fst res) i

decide :: Int -> Int -> (Int, Char)
decide v i =
  let diff = v - i
  in case () of
  _  | diff == 0 -> (diff, '+')
     | v `mod` i == 0 -> (v `div` i, '*')
     | otherwise -> (diff, '+')
