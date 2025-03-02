import Control.Arrow
import Data.Function
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

width = 5 :: Int
height = 5 :: Int

main :: IO ()
main = let scheme = "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####\n\n"
  in case parse tables "" scheme of
    Left err -> putStrLn $ show err
    Right res -> putStrLn . show $ res --castScheme <$> res

-- logic

castScheme :: [[Char]] -> Scheme
castScheme tb
  | uncurry (&&) $ (checkRow '#' . head &&& checkRow '.' . last) tb = Key (countScheme tb)
  | uncurry (&&) $ (checkRow '.' . head &&& checkRow '#' . last) tb = Lock (countScheme tb)
  | otherwise = CorruptedScheme

countScheme :: [[Char]] -> [Int]
countScheme tb = let body = init $ tail tb
  in countColumn <$> transpose body
  
countColumn :: [Char] -> Int
countColumn = length . filter (== '#')

data Scheme = Key [Int] | Lock [Int] | CorruptedScheme
  deriving Show

checkRow :: Char -> ([Char] -> Bool)
checkRow c = (== replicate width c)

-- parsing

tables :: Parser [[[Char]]]
tables = many table <* eof

table :: Parser [[Char]]
table = many row <* newline

row :: Parser [Char]
row = many (choice [char '#', char '.']) <* newline
