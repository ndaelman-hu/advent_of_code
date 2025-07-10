import Linear.V2
import Data.Bifunctor (bimap)
import Data.List (nubBy)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Pos (sourceLine, sourceColumn)

main :: IO ()
main = do
  let inputs = [('a', V2 3 4), ('a', V2 4 8), ('a', V2 5 5)]
  content <- readFile "problem.txt"
  let parsed = parse pMain "" content
  print inputs
  case parsed of
    Left err  -> print err
    Right res -> print res
  -- print . fullFilter inputs . concat $ pairAp compAntiNodes inputs

fullFilter :: [Node] -> [Node] -> [Node]
fullFilter ogs = 
  filter (inBounds (V2 10 10) . snd) . -- filter for antinodes inside the boundaries
  nubBy (\x y -> snd x == snd y) . -- filter for unique positions
  filter (\x -> snd x `notElem` fmap snd ogs) -- filter for antinodes, i.e. no originals

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 rx ry) (V2 x y) = x < rx && y < ry 

pairAp :: Applicative f => (a -> a -> b) -> f a -> f b
pairAp f x = f <$> x <*> x

compAntiNodes :: Node -> Node -> [Node]
compAntiNodes (k, v) (l, w) =
  if k == l
    then [(k, v + d), (l, w - d)]
    else []
  where d = v - w

type Node = (Char, V2 Int)

-- parsing

pMain :: Parser ([Node], V2 Int) -- nodes and boundaries
pMain = do
  ns  <- unzip <$> many pNodeEnd
  return (Data.Bifunctor.bimap concat (foldl (+) (V2 0 0)) ns)

pNodeEnd :: Parser ([Node], V2 Int)
pNodeEnd = do
  n <- pNodeLine
  p <- getPosition
  _ <- newline
  e <- optionMaybe eof
  case e of
    Nothing -> return (n, V2 0 0)
    Just r  -> return (n, sourceToPos p)

pNodeLine :: Parser [Node]
pNodeLine = concat <$> manyTill (try (return <$> pNode) <|> (char '.' >> return [])) (lookAhead newline)

pNode :: Parser Node
pNode = do
  c <- alphaNum
  p <- getPosition
  return (c, sourceToPos p)

sourceToPos :: SourcePos -> V2 Int
sourceToPos s = V2 (sourceLine s - 1) (sourceColumn s - 2)
