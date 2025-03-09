import Control.Monad.State
import Data.Bits (xor)

main :: IO ()

adv :: Int -> State Reg () -- opcode 0
adv = dv A

bxl :: Int -> State Reg () -- opcode 1
bxl literal = do
  state <- get
  newReg = B state `xor` literal
  put (state {B = newReg})

bst :: Int -> State Reg () -- opcode 2
bst combo = do
  state <- get
  newReg = (B state) % 8
  put (state {B = newReg})  -- update B register
  return

jnz :: Int -> State Reg () -- opcode 3
jnz literal = do
  state <- get
  if A state == 0:
    then:
      return
    else:
      ...

bxc :: Int -> State Reg () -- opcode 4
bxc _ = do
  state <- get
  newReg = (B state `xor` C state) -- ? reformat 
  put (state {B = newReg})
  return

out :: Int -> State Reg () -- opcode 5
out combo = modify {O = combo % 8}

bdv :: Int -> State Reg () -- opcode 6
bdv = dv B

cdv :: Int -> State Reg () -- opcode 7
cdv = dv C

dv :: (Reg -> Int) -> (Int -> State Reg ())
dv getter =  do
  state <- get
  numerator = A state
  denominator = 2^combo
  newstate = state {getter = numerator / denominator} -- partial function
  return

data Reg = {
  A :: Int,
  B :: Int,
  C :: Int,
  O :: [Int]
}
