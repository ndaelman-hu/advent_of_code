import Data.Massiv.Array as A
import Control.Comonad
import Linear.V2
import Data.Maybe (catMaybes)
import Prelude hiding (reverse)
import qualified Prelude as P

type Grid a = Array U Ix2 a

countMatch :: String -> Grid Char -> Int
countMatch patt gr =
  P.sum $ [checkGen genHorz id, checkGen genHorz P.reverse, checkGen genVert id, checkGen genVert P.reverse, checkGen genDiag id, checkGen genDiag P.reverse] <*> [[0..3]] <*> [patt] <*> [gr]

checkGen :: ([Int] -> Grid Char -> [Maybe Char]) -> ([Char] -> [Char]) -> [Int] -> [Char] -> Grid Char -> Int
checkGen gf mn gen ref gr = if mn (catMaybes (gf gen gr)) == ref then 1 else 0

genHorz :: [Int] -> Grid Char -> [Maybe Char]
genHorz ind gr = P.map (\i -> A.index' gr (Ix2 0 i)) ind

genVert :: [Int] -> Grid Char -> [Maybe Char] 
genVert ind gr = P.map (\j -> A.index' gr (Ix2 j 0)) ind

genDiag :: [Int] -> Grid Char -> [Maybe Char]
genDiag ind gr = P.map (\i -> A.index' gr (Ix2 i i)) ind
