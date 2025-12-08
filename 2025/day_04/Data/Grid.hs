module Data.Grid
  ( Grid
  , fromLists
  , toLists
  , dimensions
  , get
  , set
  , indices
  , elems
  , assocs
  , map
  , mapWithIndex
  ) where

import Prelude hiding (map)
import Data.Array (Array, array, bounds, (!), (//), assocs, elems, listArray)
import qualified Data.Array as A

-- | A 2D grid indexed by (row, col) starting at (0, 0)
type Grid a = Array (Int, Int) a

-- | Create a grid from a list of lists (rows)
fromLists :: [[a]] -> Grid a
fromLists [] = error "fromLists: empty list"
fromLists rows@(r:rs)
  | not (all ((== width) . length) rs) = error "fromLists: rows have different lengths"
  | otherwise = array ((0, 0), (height - 1, width - 1)) indexed
  where
    height = length rows
    width = length r
    indexed = [((row, col), cell)
              | (row, rowData) <- zip [0..] rows
              , (col, cell) <- zip [0..] rowData]

-- | Convert a grid back to a list of lists
toLists :: Grid a -> [[a]]
toLists grid = [[grid ! (r, c) | c <- [minC..maxC]] | r <- [minR..maxR]]
  where
    ((minR, minC), (maxR, maxC)) = bounds grid

-- | Get dimensions (rows, cols)
dimensions :: Grid a -> (Int, Int)
dimensions grid = (maxR - minR + 1, maxC - minC + 1)
  where
    ((minR, minC), (maxR, maxC)) = bounds grid

-- | Safely get an element at (row, col)
get :: Grid a -> (Int, Int) -> Maybe a
get grid pos
  | A.inRange (bounds grid) pos = Just (grid ! pos)
  | otherwise = Nothing

-- | Set an element at (row, col)
set :: Grid a -> (Int, Int) -> a -> Grid a
set grid pos val = grid // [(pos, val)]

-- | Get all valid indices in the grid
indices :: Grid a -> [(Int, Int)]
indices grid = A.indices grid

-- | Map a function over all elements
map :: (a -> b) -> Grid a -> Grid b
map f grid = listArray (bounds grid) [f x | x <- elems grid]

-- | Map a function over all elements with their indices
mapWithIndex :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
mapWithIndex f grid = A.array (bounds grid) [(i, f i (grid ! i)) | i <- indices grid]
