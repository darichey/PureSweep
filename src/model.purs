module Model
  ( Field(..)
  , Cell(..)
  , UnderlyingCellState
  , PlayerState
  , debugField
  , makeField
  , makeRandomField
  )
  where

import Prelude

import Control.Monad.ST as ST
import Data.Array (concat, replicate, (..))
import Data.Array.ST as STArray
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.String (joinWith)
import Effect (Effect)
import Sample (sample)
import Split (chunksOf)

data UnderlyingCellState
  = Mine
  | Safe Int

instance Show UnderlyingCellState where
  show Mine = "*"
  show (Safe nearby) = show nearby

data PlayerState
  = Open
  | Closed
  | Flagged

data Cell
  = Cell { underlying :: UnderlyingCellState, player :: PlayerState }

data Field
  = Field
    { cells :: Array Cell
    , width :: Int
    , height :: Int
    }

debugField :: Field -> String
debugField (Field field) = joinWith "\n" (map (\row -> joinWith "" row) (chunksOf field.width (map (\(Cell cell) -> show cell.underlying) (field.cells))))

makeField :: Int -> Int -> HashSet Int -> Field
makeField width height mineIndices = Field { cells, width, height }
  where
    cells = STArray.run (do
      array <- STArray.unsafeThaw $ replicate (width * height) (Cell { underlying: Safe 0, player: Closed })
      _ <- ST.foreach (HashSet.toArray mineIndices) (\i ->        
        do
          _ <- STArray.poke i (Cell { underlying: Mine, player: Closed }) array
          ST.foreach (neighbors i) (\j -> void $ STArray.modify j incNearby array)
      )
      pure array
    )

    incNearby :: Cell -> Cell
    incNearby (Cell { underlying: (Safe n), player }) = Cell { underlying: (Safe (n + 1)), player }
    incNearby x = x

    neighbors :: Int -> Array Int
    neighbors index = concat [
      -- if the column to the right is in bounds, add the three neighbors from it.
      if (index `mod` width) + 1 < width then [index + 1, index + width + 1, index - width + 1] else [],
      -- if the column to the left is in bounds, add the three neighbors from it.
      if (index `mod` width) - 1 > 0 then [index - 1, index + width - 1, index - width - 1] else [],
      -- unconditionally add the neighbors above and below. STArray.modify does bounds checking, and
      -- the grid doesn't wrap on the top and bottom, so it doesn't matter if these exist or not.
      [index + width, index - width]
    ]

makeRandomField :: Int -> Int -> Int -> Effect Field
makeRandomField width height numMines = do
  mineIndices <- sample (0 .. (width * height - 1)) numMines
  pure $ makeField width height mineIndices
  