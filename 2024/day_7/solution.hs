main :: IO ()
main = (putStrLn . show . sum) $
  (\(v, is) -> selForSum (deconstruct "" v is) v) <$>
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

deconstruct :: String -> Int -> [Int] -> (Int, String)
deconstruct chs v is = if v <= 0 || is == []
  then (v, tail chs)
  else
    let (iss, i) = (init is, last is); new = decide v i
    in deconstruct ((snd new) : (show i) ++ chs) (fst new) iss

decide :: Int -> Int -> (Int, Char)
decide v i =
  let diff = v - i
  in case () of
  _  | diff == 0 -> (diff, '+')
     | v `mod` i == 0 -> (v `div` i, '*')
     | otherwise -> (diff, '+')
