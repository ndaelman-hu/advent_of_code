import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad (ap)
import Data.Bits (xor)
import Debug.Trace (trace)

main :: IO ()
main = putStrLn . show . tapeO $ final
  where 
    final = until (liftA2 (||) isHalted tapeAtEnd) debugInstr prog
    prog = Prog {
      regA = 729,
      regB = 0,
      regC = 0,
      headPos = 0,
      tapeI = [0,1,5,4,3,0],
      tapeO = [],
      isHalted = False
    }

debugInstr :: Prog -> Prog
debugInstr p = 
  let p' = instr p
      debug = "Head: " ++ show (headPos p) ++
              " OpCode: " ++ show (safeReadOpcode p) ++ 
              " OpArg: " ++ show (safeReadOperand p) ++ 
              " RegA: " ++ show (regA p) ++
              " RegB: " ++ show (regB p) ++ 
              " RegC: " ++ show (regC p)
  in trace debug p'

instr :: Prog -> Prog
instr p = 
  let opCode = safeReadOpcode p
      opArg = safeReadOperand p
  in opcode opCode opArg p

safeReadOpcode :: Prog -> Int
safeReadOpcode p 
  | headPos p < length (tapeI p) = tapeI p !! headPos p
  | otherwise = -1  -- halt when out of bounds

safeReadOperand :: Prog -> Int
safeReadOperand p
  | headPos p + 1 < length (tapeI p) = tapeI p !! (headPos p + 1)
  | otherwise = -1

opcode :: Int -> Int -> Prog -> Prog
opcode 0 n p = jmp 2 $ adv n p
opcode 1 n p = jmp 2 $ bxl n p
opcode 2 n p = jmp 2 $ bst n p
opcode 3 n p = jnz n p  -- jnz handles jumps internally
opcode 4 n p = jmp 2 $ bxc n p
opcode 5 n p = jmp 2 $ out n p
opcode 6 n p = jmp 2 $ bdv n p
opcode 7 n p = jmp 2 $ cdv n p
opcode _ _ p = halt p  -- halt when out of bounds

bst :: Int -> Prog -> Prog
bst n p = p {regB = (operand True n p) `mod` 8}

jnz :: Int -> Prog -> Prog
jnz n p = if regA p == 0
  then jmp 2 p
  else p {headPos = (operand False n p)}

out :: Int -> Prog -> Prog
out n p = p {tapeO = tapeO p ++ [(operand True n p) `mod` 8]}

bxl :: Int -> Prog -> Prog
bxl n p = bx (regB p) (operand False n p) p

bxc :: Int -> Prog -> Prog
bxc _ p = bx (regB p) (regC p) p

bx :: Int -> Int -> Prog -> Prog
bx m n p = p {regB = m `xor` n}

adv :: Int -> Prog -> Prog
adv = dv regA

bdv :: Int -> Prog -> Prog
bdv = dv regB

cdv :: Int -> Prog -> Prog
cdv = dv regC

dv :: (Prog -> Int) -> Int -> Prog -> Prog
dv getter n p = 
  let divisor = 2 ^ (operand True n p)
  in p {regA = getter p `div` divisor}

tapeAtEnd :: Prog -> Bool
tapeAtEnd p = headPos p >= length (tapeI p)

halt :: Prog -> Prog
halt p = p {isHalted = True}

operand :: Bool -> Int -> (Prog -> Int)
operand combo n
  | combo && n `elem` [0,1,2,3] = const n
  | combo && n == 4 = regA
  | combo && n == 5 = regB
  | combo && n == 6 = regC
  | combo && n == 7 = const (-1) -- halt
  | otherwise = const n

readOpcode :: Prog -> Int
readOpcode p = tapeI p !! headPos p

readOperand :: Prog -> Int
readOperand p = tapeI p !! (headPos p + 1)

jmp :: Int -> Prog -> Prog
jmp n p = p {headPos = headPos p + n}

data Prog = Prog {
  regA :: Int,
  regB :: Int,
  regC :: Int,
  headPos :: Int,
  tapeI :: [Int],
  tapeO :: [Int],
  isHalted :: Bool
} deriving Show
