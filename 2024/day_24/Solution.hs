import qualified Data.HashTable.ST.Basic as H
import Control.Monad.ST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (alphaNum, upper, space)

main :: IO ()
main = return ()

-- solution =  

apLogic :: String -> String -> String -> (Bool -> Bool -> Bool) -> H.HashTable s String Bool -> ST s ()
apLogic lft rght trgt oprtr ht = do
  lb <- H.lookup ht lft
  rb <- H.lookup ht rght
  case (lb, rb) of
    (Just lbb, Just rbb) -> H.insert ht trgt $ oprtr lbb rbb
    (_, _) -> return ()

-- parsing

var :: Parser (String, Bool)
var = do
  vname <- count 3 alphaNum
  _     <- string ": "
  val   <- (== '1') <$> oneOf "01"
  return (vname, val)

gate :: Parser (String, String, String, Bool -> Bool -> Bool)
gate = do
  lname <- count 3 alphaNum
  _     <- space
  op    <- many1 upper
  _     <- space
  rname <- count 3 alphaNum
  _     <- string " -> "
  trgt  <- count 3 alphaNum
  case op of
    "AND" -> return (lname, rname, trgt, (&&))
    "OR"  -> return (lname, rname, trgt, (||))
    "XOR" -> return (lname, rname, trgt, (/=))
    _     -> error "non-supported logic operation"
