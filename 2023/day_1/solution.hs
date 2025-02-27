import Data.Char
import Data.Either (rights)
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main =  (putStrLn . show . sum) $ readLine <$> ["1abc2"
                                   , "pqr3stu8vwx"
                                   , "a1b2c3d4e5f"
                                   , "treb7uchet"
                                   ]

readLine :: String -> Int
readLine s = read $ rights [parse first_num "" (f s) | f <- [id, reverse]]

first_num :: Parser Char
first_num = skipMany (satisfy (not . isDigit)) *> digit
