import Data.List (sortOn)
import qualified Data.HashTable.ST.Basic as H
import Data.HashTable.Class (toList)
import Control.Monad.ST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (alphaNum, upper, space)

main :: IO ()
main = do
  cvars  <- readFile "vars.txt"
  cgates <- readFile "gates.txt"
  let vars  = parse (endBy pvar newline)  "" cvars
  let gates = parse (endBy pgate newline) "" cgates
  case (vars, gates) of
    (Right evars, Right egates) -> putStr . svar . sortOn fst $ solution evars egates
    _ -> print "Failure"

solution :: [Var] -> [Gate] -> [Var]
solution vars gates = runST $ do
  ht <- H.new
  mapM_ (uncurry $ H.insert ht) vars 
  compGates gates ht
  toList ht

compGates :: [Gate] -> H.HashTable s String Bool -> ST s ()
compGates gates ht = do
  oldLength <- length <$> toList ht
  mapM_ (`apLogic` ht) gates 
  newLength <- length <$> toList ht
  if oldLength == newLength
     then return ()
     else compGates gates ht

apLogic :: Gate -> H.HashTable s String Bool -> ST s ()
apLogic (lft, rght, trgt, oprtr) ht = do
  lb <- H.lookup ht lft
  rb <- H.lookup ht rght
  case (lb, rb) of
    (Just lbb, Just rbb) -> H.insert ht trgt $ oprtr lbb rbb
    (_, _) -> return ()

type Var = (String, Bool)
type Gate = (String, String, String, Bool -> Bool -> Bool) -- left I, right I, target O

-- parsing

svar :: [Var] -> String
svar = unlines . map stringFromVar
  where
    stringFromVar (k, v) = k ++ ": " ++ show (intFromBool v)
    intFromBool b = if b then 1 else 0

pvar :: Parser Var
pvar = do
  vname <- count 3 alphaNum
  _     <- string ": "
  val   <- (== '1') <$> oneOf "01"
  return (vname, val)

pgate :: Parser Gate 
pgate = do
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
