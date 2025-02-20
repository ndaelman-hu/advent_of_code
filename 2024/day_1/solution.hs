import Data.List (sort)
import Data.List (foldl')

main :: IO ()
main = putStrLn $ show $ solution [3, 4, 2, 1, 3, 3] [4, 3, 5, 3, 9, 3]

solution :: [Int] -> [Int] -> Int
solution listl listr = foldl' (+) 0 $ abs <$> zipWith (-) (sort listl) (sort listr)
