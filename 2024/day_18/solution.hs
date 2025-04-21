import Data.List (stripPrefix, partition, sortBy)

main :: IO ()
main = putStrLn . show $ recurse sorted "brwrr" []
  where
    sorted = sortBy (\x y -> flip compare (length x) (length y)) patts
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
