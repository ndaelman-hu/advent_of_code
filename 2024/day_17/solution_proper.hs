import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad (ap)
import Data.Bits (xor)
import Debug.Trace (trace)

main :: IO ()
main = putStrLn . show . tapeO $ final
  where 
    final = until (liftA2 (||) isHalted tapeAtEnd) instr prog
    prog = Prog {
      regA = 729,
      regB = 0,
      regC = 0,
      headPos = 0,
      tapeI = [0,1,5,4,3,0],
      tapeO = [],
      isHalted = False
    }

-- Add debug to see what's happening
step :: Prog -> Prog
step p = 
  let p' = instr p
      debug = "Step: " ++ show (headPos p) ++ " OpCode: " ++ show (safeReadOpcode p) ++ 
              " OpArg: " ++ show (safeReadOperand p) ++ 
              " RegA: " ++ show (regA p) ++ " RegB: " ++ show (regB p) ++ 
              " RegC: " ++ show (regC p)
  in trace debug p'

instr :: Prog -> Prog
instr p = 
  let opCode = safeReadOpcode p
      opArg = safeReadOperand p
  in opcode opCode opArg p

-- Safe version to prevent out-of-bounds errors
safeReadOpcode :: Prog -> Int
safeReadOpcode prog 
  | headPos prog < length (tapeI prog) = tapeI prog !! headPos prog
  | otherwise = -1  -- Use -1 for halt when out of bounds

safeReadOperand :: Prog -> Int
safeReadOperand prog
  | headPos prog + 1 < length (tapeI prog) = tapeI prog !! (headPos prog + 1)
  | otherwise = 0  -- Default value when out of bounds

opcode :: Int -> Int -> Prog -> Prog
opcode 0 n p = jmp 2 $ adv n p
opcode 1 n p = jmp 2 $ bxl n p
opcode 2 n p = jmp 2 $ bst n p
opcode 3 n p = jnz n p  -- jnz handles jumps internally
opcode 4 n p = jmp 2 $ bxc n p
opcode 5 n p = jmp 2 $ out n p
opcode 6 n p = jmp 2 $ bdv n p
opcode 7 n p = jmp 2 $ cdv n p
opcode _ _ p = halt p

bst :: Int -> Prog -> Prog
bst n prog = prog {regB = (operand True n prog) `mod` 8}

jnz :: Int -> Prog -> Prog
jnz n prog = if regA prog == 0
  then jmp 2 prog
  else prog {headPos = (operand False n prog)}

out :: Int -> Prog -> Prog
out n prog = prog {tapeO = tapeO prog ++ [(operand True n prog) `mod` 8]}

bxl :: Int -> Prog -> Prog
bxl n prog = bx (regB prog) (operand False n prog) prog

bxc :: Int -> Prog -> Prog
bxc _ prog = bx (regB prog) (regC prog) prog

bx :: Int -> Int -> Prog -> Prog
bx m n prog = prog {regB = m `xor` n}

adv :: Int -> Prog -> Prog
adv = dv regA

bdv :: Int -> Prog -> Prog
bdv = dv regB

cdv :: Int -> Prog -> Prog
cdv = dv regC

dv :: (Prog -> Int) -> Int -> Prog -> Prog
dv getter n prog = 
  let divisor = 2 ^ (operand True n prog)
  in if divisor == 0 
     then prog  -- Prevent division by zero
     else prog {regA = getter prog `div` divisor}

tapeAtEnd :: Prog -> Bool
tapeAtEnd prog = headPos prog >= length (tapeI prog)

halt :: Prog -> Prog
halt prog = prog {isHalted = True}

operand :: Bool -> Int -> (Prog -> Int)
operand combo n
  | combo && n `elem` [0,1,2,3] = const n
  | combo && n == 4 = regA
  | combo && n == 5 = regB
  | combo && n == 6 = regC
  | combo && n == 7 = const (-1) -- halt
  | otherwise = const n

readOpcode :: Prog -> Int
readOpcode prog = tapeI prog !! headPos prog

readOperand :: Prog -> Int
readOperand prog = tapeI prog !! (headPos prog + 1)

jmp :: Int -> Prog -> Prog
jmp n prog = prog {headPos = headPos prog + n}

data Prog = Prog {
  regA :: Int,
  regB :: Int,
  regC :: Int,
  headPos :: Int,
  tapeI :: [Int],
  tapeO :: [Int],
  isHalted :: Bool
} deriving Show
