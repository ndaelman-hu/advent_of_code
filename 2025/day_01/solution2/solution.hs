import Debug.Trace (traceM)
import Control.Monad.State.Strict
import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile dial "input.txt"
  case inp of
    Left e -> print e
    Right r -> let (_, res) = runState (mapM countClick r) (50, 0) 
                in print res

-- monad

countClick :: Int -> Control.Monad.State.Strict.State (Int, Int) () -- rotation -> State (dial, clicks, direction)
countClick rot = do
  (dial, clicks) <- get
  traceM ("Current state: " ++ show (dial, clicks))
  let rawRot = quot rot 100
  let leftOverRot = rem rot 100
  let newDial = rem (dial + leftOverRot) 100
  let extraClick = if signum dial == signum newDial || signum dial == 0 then 0 else 1
  put (newDial, clicks + abs rawRot + extraClick)

-- trials

f :: [Int] -> [Int]
f = scanl (-) (-50)

cntSgnChanges :: [Int] -> [Int]
cntSgnChanges xs = zipWith (*) (fmap signum xs) (signum <$> tail xs)

fullRot :: [Int] -> [Int] 
fullRot = fmap (`div` 100)

-- parsing

rotation :: Parser Int
rotation = do
  sgn <- char 'L' $> (* (-1)) <|> char 'R' $> id
  n <- many1 digit
  return $ sgn (read n)

dial :: Parser [Int]
dial = many (rotation <* newline)
