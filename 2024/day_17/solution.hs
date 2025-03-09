import Control.Monad.State
import Data.Bits (xor)

main :: IO ()

adv :: [Int] -> State Reg [Int] -- opcode 0
adv = dv A

bxl :: [Int] -> State Reg [Int] -- opcode 1
bxl literals = bx (literals !! 1)

bst :: [Int] -> State Reg [Int] -- opcode 2
bst _ = do
  state <- get
  newReg = (B state) % 8
  put (state {B = newReg})  -- update B register
  jmp 2

jnz :: [Int] -> State Reg [Int] -- opcode 3
jnz literals = do
  state <- get
  if A state == 0:
    then:
      jmp 2 
    else:
      jmp (literals !! 1)

bxc :: [Int] -> State Reg [Int] -- opcode 4
bxc = get >>= (\state -> bx (C state))

out :: [Int] -> State Reg [Int] -- opcode 5
out combos = modify {O = (combos !! 1) % 8} >>=  jmp 2

bdv :: [Int] -> State Reg [Int] -- opcode 6
bdv = dv B

cdv :: [Int] -> State Reg [Int] -- opcode 7
cdv = dv C

dv :: (Reg -> Int) -> ([Int] -> State Reg [Int])
dv getter = do
  state <- get
  numerator = A state
  denominator = 2^combo
  newstate = state {getter = numerator / denominator} -- partial function
  jmp 2

bx :: Int -> ([Int] -> State Reg [Int])
bx num = do
  state <- get
  newReg = B state `xor` num 
  put (state {B = newReg})
  jmp 2

jmp :: Int -> ([Int] -> State Reg [Int]) -- or add to state?
jmp step = return (drop step list)

data Reg = {
  A :: Int,
  B :: Int,
  C :: Int,
  O :: [Int]
}
