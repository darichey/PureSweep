module Model
  ( Cell
  , Field
  , PlayerState(..)
  , RevealResult(..)
  , UnderlyingCellState(..)
  , debugField
  , isOpen
  , makeField
  , makeRandomField
  , revealAt
  , toggleFlagAt
  )
  where

import Prelude

import Control.Monad.ST as ST
import Data.Array (concat, modifyAt, replicate, (!!), (..))
import Data.Array.ST as STArray
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Maybe (Maybe(..), fromJust)
import Data.String (joinWith)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
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
  | Flag

type Cell = { underlying :: UnderlyingCellState, player :: PlayerState }

makeOpen :: Cell -> Cell
makeOpen cell = cell { player = Open }

isOpen :: Cell -> Boolean
isOpen { player: Open } = true
isOpen _ = false

toggleFlag :: Cell -> Cell
toggleFlag cell@{ player: Closed } = cell { player = Flag }
toggleFlag cell@{ player: Flag } = cell { player = Closed }
toggleFlag cell = cell

type Field = { cells :: Array Cell, width :: Int, height :: Int }

debugField :: Field -> String
debugField field = joinWith "\n" (map (\row -> joinWith "" row) (chunksOf field.width (map (\cell -> show cell.underlying) (field.cells))))

makeField :: Int -> Int -> HashSet Int -> Field
makeField width height mineIndices = { cells, width, height }
  where
    cells = STArray.run (do
      array <- STArray.unsafeThaw $ replicate (width * height) { underlying: Safe 0, player: Closed }
      ST.foreach (HashSet.toArray mineIndices) (\i ->        
        do
          void $ STArray.poke i { underlying: Mine, player: Closed } array
          ST.foreach (neighbors i) (\j -> void $ STArray.modify j incNearby array)
      )
      pure array
    )

    incNearby :: Cell -> Cell
    incNearby cell@{ underlying: (Safe n) } = cell { underlying = (Safe (n + 1)) }
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

data RevealResult = Ok Field | Explode

revealAt :: Int -> Field -> RevealResult
revealAt i field = case field.cells !! i of
  -- if it was out of bounds somehow, already open, or a flag, just no-op
  Nothing -> Ok field
  Just { player: Open } -> Ok field
  Just { player: Flag } -> Ok field
  -- if it's safe, return the field with the cell revealed
  Just { underlying: Safe _ } -> Ok $ field { cells = unsafePartial $ fromJust $ modifyAt i makeOpen field.cells }
  -- otherwise... oops
  Just { underlying: Mine } -> Explode

toggleFlagAt :: Int -> Field -> Maybe Field
toggleFlagAt i field = field { cells = _ } <$> modifyAt i toggleFlag field.cells
