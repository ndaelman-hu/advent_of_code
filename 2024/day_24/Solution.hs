import Control.Monad.ST
import Data.List (sortOn, isPrefixOf)
import qualified Data.HashTable.ST.Basic as H
import Data.HashTable.Class (toList)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (alphaNum, upper, space)
import Data.Char (digitToInt)

main :: IO ()
main = do
  cvars  <- readFile "vars.txt"
  cgates <- readFile "gates.txt"
  let vars  = parse (endBy pvar newline)  "" cvars
  let gates = parse (endBy pgate newline) "" cgates
  case (vars, gates) of
    (Right evars, Right egates) -> do
      let results =  sortOn fst $ solution evars egates
      let resultsZ = filter (isPrefixOf "z" . fst) results -- vars named z**
      let binariesZ  = concatMap (show . intFromBool . snd) resultsZ -- vars values combined into a binary number
      let numeralsZ = intFromBinary . map snd . reverse $ resultsZ -- numeral representation of binary number
      putStr $ svar results ++ "\nDecoded: " ++ binariesZ ++ ", " ++ show numeralsZ
    _ -> print "Failure"

solution :: [Var] -> [Gate] -> [Var]
solution vars gates = runST $ do
  ht <- H.new
  mapM_ (uncurry $ H.insert ht) vars 
  convLogic apLogic gates ht
  mapM_ (H.delete ht) $ fmap fst vars -- delete original vars
  toList ht

convLogic :: (a -> H.HashTable s k v -> ST s Bool) -> [a] -> H.HashTable s k v -> ST s ()
convLogic op xs ht = do
  passed <- mapM (`op` ht) xs 
  let (_, ys) = unzip $ filter (not . fst) $ zip passed xs -- filter out passed gates
  if null ys
     then return ()
     else convLogic op ys ht

apLogic :: Gate -> H.HashTable s String Bool -> ST s Bool
apLogic (lft, rght, trgt, oprtr) ht = do
  lb <- H.lookup ht lft
  rb <- H.lookup ht rght
  case (lb, rb) of
    (Just lbb, Just rbb) -> do
      H.insert ht trgt $ oprtr lbb rbb
      return True
    (_, _) -> return False

-- converters

intFromBinary :: [Bool] -> Int
intFromBinary bools = sum [if b then 2^i else 0 | (i, b) <- zip [0..] (reverse bools)]

svar :: [Var] -> String
svar = unlines . map stringFromVar
  where stringFromVar (k, v) = k ++ ": " ++ show (intFromBool v)

intFromBool :: Bool -> Int
intFromBool b = if b then 1 else 0

type Var = (String, Bool)
type Gate = (String, String, String, Bool -> Bool -> Bool) -- left I, right I, target O

-- parsing

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
