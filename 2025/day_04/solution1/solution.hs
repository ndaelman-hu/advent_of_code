import Data.Grid
import Data.Grid.Convolution
import Text.Parsec hiding (count)
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile parseGrid "input.txt"
  case inp of
    Left e -> print e
    Right r -> print $ f r

f :: Grid Char -> Int
f = autoConvolute omitBounds fullCard
  where fullCard window =
          let center = window !! 1 !! 1  -- center of 3x3 window
              flatWindow = concat window
           in center == Just '@' && count (Just '@') flatWindow < 5

count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

parseGrid :: Parser (Grid Char)
parseGrid = do
  rows <- many1 (many1 (noneOf "\n") <* newline) <* eof
  return $ Data.Grid.fromLists rows
