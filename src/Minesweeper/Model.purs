module Minesweeper.Model
  ( Cell
  , CellIndex(..)
  , Dims
  , Field
  , Height
  , PlayerState(..)
  , STField
  , UnderlyingCellState(..)
  , Width
  , getNeighbors
  , incNearby
  , isClosed
  , isFlag
  , makeField
  , makeOpen
  , makeRandomField
  , toggleFlag
  ) where

import Prelude

import Control.Monad.ST (Region)
import Control.Monad.ST as ST
import Data.Array (catMaybes, replicate, (..))
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Sample (sample)

type Width = Int
type Height = Int
type CellIndex = Int

data UnderlyingCellState
  = Mine
  | Safe Int

instance Show UnderlyingCellState where
  show Mine = "*"
  show (Safe nearby) = show nearby

data PlayerState
  = Open
  | Closed
  | Flag

type Cell = { underlying :: UnderlyingCellState, player :: PlayerState }

makeOpen :: Cell -> Cell
makeOpen cell = cell { player = Open }

isClosed :: Cell -> Boolean
isClosed { player: Closed } = true
isClosed _ = false

isFlag :: Cell -> Boolean
isFlag { player: Flag } = true
isFlag _ = false

incNearby :: Cell -> Cell
incNearby cell@{ underlying: (Safe n) } = cell { underlying = (Safe (n + 1)) }
incNearby x = x

toggleFlag :: Cell -> Cell
toggleFlag cell@{ player: Closed } = cell { player = Flag }
toggleFlag cell@{ player: Flag } = cell { player = Closed }
toggleFlag cell = cell

type Dims = { width :: Width, height :: Height }

type STField :: Region -> Type
type STField h = { cells :: STArray h Cell, dims :: Dims }

type Field = { cells :: Array Cell, dims :: Dims }

getNeighbors :: CellIndex -> Width -> Height -> Array CellIndex
getNeighbors index width height =
  catMaybes
    [ if (columnLeft && rowAbove) then Just (index - width - 1) else Nothing
    , if (rowAbove) then Just (index - width) else Nothing
    , if (columnRight && rowAbove) then Just (index - width + 1) else Nothing
    , if (columnLeft) then Just (index - 1) else Nothing
    , if (columnRight) then Just (index + 1) else Nothing
    , if (columnLeft && rowBelow) then Just (index + width - 1) else Nothing
    , if (rowBelow) then Just (index + width) else Nothing
    , if (columnRight && rowBelow) then Just (index + width + 1) else Nothing
    ]
  where
  columnLeft = (index `mod` width) - 1 >= 0
  columnRight = (index `mod` width) + 1 < width
  rowAbove = index - width >= 0
  rowBelow = (index + width) `div` width < height

makeField :: Width -> Height -> HashSet CellIndex -> Field
makeField width height mineIndices = { cells, dims: { width, height } }
  where
  cells = STArray.run
    ( do
        array <- STArray.unsafeThaw $ replicate (width * height) { underlying: Safe 0, player: Closed }
        ST.foreach (HashSet.toArray mineIndices)
          ( \i ->
              do
                void $ STArray.poke i { underlying: Mine, player: Closed } array
                ST.foreach (getNeighbors i width height) (\j -> void $ STArray.modify j incNearby array)
          )
        pure array
    )

makeRandomField :: Width -> Height -> Int -> Effect Field
makeRandomField width height numMines = do
  mineIndices <- sample (0 .. (width * height - 1)) numMines
  pure $ makeField width height mineIndices
