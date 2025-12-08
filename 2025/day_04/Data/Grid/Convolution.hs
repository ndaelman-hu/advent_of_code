module Data.Grid.Convolution
  ( Window
  , autoConvolute
  , convolute
  , getWindow
  , neighbors4
  , neighbors8
  , omitBounds
  , includeBounds
  ) where

import Data.Grid (Grid, get, dimensions)
import Data.Array (bounds, (!))
import qualified Data.Array as A

-- | A window is a small grid centered around a position
type Window a = [[Maybe a]]

-- | Get a 3x3 window centered at (row, col)
getWindow :: Grid a -> (Int, Int) -> Window a
getWindow grid (r, c) =
  [[get grid (r+dr, c+dc) | dc <- [-1..1]] | dr <- [-1..1]]

-- | Convolute over a grid with a predicate function on windows
-- Returns a list of positions where the predicate is True
convolute :: (Window a -> Bool) -> Grid a -> [(Int, Int)]
convolute predicate grid =
  [pos | pos <- A.indices grid, predicate (getWindow grid pos)]

-- | Auto-convolute: applies a boundary policy and counts matches
-- omitBounds: only check positions not on the boundary
-- predicate: function that checks if window matches pattern
autoConvolute :: ((Int, Int) -> (Int, Int) -> Bool) -> (Window a -> Bool) -> Grid a -> Int
autoConvolute boundPolicy predicate grid =
  length $ filter (\pos -> boundPolicy pos dims && predicate (getWindow grid pos)) (A.indices grid)
  where
    dims = dimensions grid

-- | Boundary policy: omit positions on the edge
omitBounds :: (Int, Int) -> (Int, Int) -> Bool
omitBounds (r, c) (rows, cols) =
  r > 0 && r < rows - 1 && c > 0 && c < cols - 1

-- | Boundary policy: include all positions
includeBounds :: (Int, Int) -> (Int, Int) -> Bool
includeBounds _ _ = True

-- | Get 4-directional neighbors (up, down, left, right)
neighbors4 :: (Int, Int) -> [(Int, Int)]
neighbors4 (r, c) = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

-- | Get 8-directional neighbors (including diagonals)
neighbors8 :: (Int, Int) -> [(Int, Int)]
neighbors8 (r, c) =
  [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]
