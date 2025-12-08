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

-- | Auto-convolute: counts matches across all positions
-- The boundary handler determines how out-of-bounds cells are treated
-- omitBounds: out-of-bounds cells become Nothing (already handled by getWindow)
-- predicate: function that checks if window matches pattern
autoConvolute :: a -> (Window b -> Bool) -> Grid b -> Int
autoConvolute _boundHandler predicate grid =
  length $ filter (predicate . getWindow grid) (A.indices grid)

-- | Boundary handler: out-of-bounds cells become Nothing (no-op, already done by getWindow)
omitBounds :: ()
omitBounds = ()

-- | Boundary handler placeholder
includeBounds :: ()
includeBounds = ()

-- | Get 4-directional neighbors (up, down, left, right)
neighbors4 :: (Int, Int) -> [(Int, Int)]
neighbors4 (r, c) = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

-- | Get 8-directional neighbors (including diagonals)
neighbors8 :: (Int, Int) -> [(Int, Int)]
neighbors8 (r, c) =
  [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]
