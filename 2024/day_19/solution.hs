import Data.List (stripPrefix, partition, sortBy)

main :: IO ()
main = print $ recurse s "brwrr" []
  where
    s = sorted patts
    patts = ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]

recurse :: [String] -> String -> [String] -> [String]
recurse patts dsgn strg
  | null dsgn = strg
  | otherwise = case consumeAny patts dsgn of
    Just (match, rest) -> recurse patts rest (strg ++ [match])
    Nothing -> strg

consumeAny :: [String] -> String -> Maybe (String, String)
consumeAny [] _ = Nothing
consumeAny (p:ps) dsgn =   
  case stripPrefix p dsgn of
    Just rest -> Just (p, rest)
    Nothing -> consumeAny ps dsgn

data Patts = Patts String [[String]]

instance Show Patts where
  show (Patts base patts) = show base ++ "(" ++ show patts ++ ")"

checkDeconstr :: Patts -> [String] -> Patts
checkDeconstr bp [] = bp
checkDeconstr (Patts base patts) exts = 
  if base == concat exts
  then Patts base (patts ++ [exts])
  else Patts base patts

sorted :: [String] -> [String]
sorted = sortBy (\x y -> compare (length y) (length x))
