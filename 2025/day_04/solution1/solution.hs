import Data.Grid
import Data.Grid.Convolution
import Text.Parsec hiding (count)
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parseGrid "input.test.txt"
  case inp of
    Left e -> print e
    Right r -> print $ f r

f :: Grid Char -> Int
f = autoConvolute omitBounds fullCard
  where fullCard grid = if get' grid (1,1) == Just '@' && count (Just '@') (concat grid) <= 5 then True else False
        get' g (r,c) = get g (r,c)  -- helper to match window coordinate system

count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

parseGrid :: Parser (Grid Char)
parseGrid = do
  rows <- many1 (many1 (noneOf "\n") <* newline) <* eof
  return $ Data.Grid.fromLists rows
