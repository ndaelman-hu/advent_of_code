import qualified Data.Grid as Grid

countMatch :: String -> Grid char -> Integer 
countMatch patt gr =
  sum . uncurry <$> checkGen genHorz id <*> checkGen genHorz reverse <*> checkGen genVert id <*> checkGen genVert reverse <*> checkGen genDiag id <*> checkGen genDiag reverse $ ([0..3], "XMAS") 

checkGen :: ([Integer] -> Grid a -> [a]) -> (a -> a) -> [Integer] -> String -> Bool 
checkGen gf mn gen ref = gr (mn gen) == ref

-- type :: [Integer] -> Grid a -> [a]
genHorz ind = experiment [V2 i 0 | i <- ind]
genVert ind = experiment [V2 0 j | j <- ind]
genDiag ind = experiment [V2 i i | i <- ind]
