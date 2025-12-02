import Debug.Trace (traceM)
import Control.Monad.State.Strict
import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  inp <- parseFromFile dial "input.test.txt"
  case inp of
    Left e -> print e
    Right r -> let (_, res) = runState (mapM countClick r) (50, 0, 0) 
                in print res

-- monad

countClick :: Int -> Control.Monad.State.Strict.State (Int, Int, Int) () -- rotation -> State (dial, clicks, direction)
countClick rot = do
  s <- get
  let (dial, clicks, dir) = s
  traceM ("Current state: " ++ show s)
  let newDial = mod (dial + rot) 100
  let newClicks = if dir == 0
                     then abs $ max (quot (dial + rot) 100) (quot (dial - rot) 100)
                      else abs $ quot (dial + dir * rot) 100
  let newDir = signum rot
  if newDial == 0
     then put (newDial, clicks + newClicks + 1, newDir)
     else put (newDial, clicks + newClicks, newDir)

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
