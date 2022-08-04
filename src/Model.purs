module Model
  ( Cell
  , CellIndex
  , Field
  , Height
  , PlayerState(..)
  , RevealResult(..)
  , UnderlyingCellState(..)
  , Width
  , chordAt
  , makeField
  , makeRandomField
  , revealAt
  , toggleFlagAt
  ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Control.Monad.ST as ST
import Data.Array (catMaybes, filter, length, modifyAt, replicate, toUnfoldable, (!!), (..))
import Data.Array.ST as STArray
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
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

type Field = { cells :: Array Cell, width :: Width, height :: Height }

makeField :: Width -> Height -> HashSet CellIndex -> Field
makeField width height mineIndices = { cells, width, height }
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

makeRandomField :: Width -> Height -> Int -> Effect Field
makeRandomField width height numMines = do
  mineIndices <- sample (0 .. (width * height - 1)) numMines
  pure $ makeField width height mineIndices

data RevealResult = Ok Field | Explode

revealAt :: CellIndex -> Field -> RevealResult
revealAt i field = revealAll [ i ] field

revealAll :: Array CellIndex -> Field -> RevealResult
revealAll indices field =
  case result of
    Nothing -> Explode
    Just newCells -> Ok $ field { cells = newCells }
  where
  result :: Maybe (Array Cell)
  result = ST.run
    ( runMaybeT do
        cells <- lift $ STArray.unsafeThaw field.cells
        _ <- sequence $ indices <#> \i -> do
          cell <- lift $ STArray.peek i cells
          case cell of
            Nothing -> pure unit
            Just { player: Open } -> pure unit
            Just { player: Flag } -> pure unit
            Just { underlying: Safe _ } -> lift $ void $ STArray.modify i makeOpen cells
            Just { underlying: Mine } -> MaybeT (pure Nothing)
        lift $ STArray.unsafeFreeze cells
    )

toggleFlagAt :: CellIndex -> Field -> Maybe Field
toggleFlagAt i field = field { cells = _ } <$> modifyAt i toggleFlag field.cells

chordAt :: CellIndex -> Field -> RevealResult
chordAt i field = case field.cells !! i of
  -- if it was out of bounds somehow, closed, or a flag, just no-op
  Nothing -> Ok field
  Just { player: Flag } -> Ok field
  Just { player: Closed } -> Ok field

  -- if it's safe, try to chord there
  Just { player: Open, underlying: Safe nearby } ->
    let
      neighbors = getNeighbors i field.width field.height
      numNearbyFlags = length $ filter (\j -> isFlag $ unsafePartial $ fromJust $ field.cells !! j) neighbors
    in
      if numNearbyFlags == nearby then
        -- do the chord by revealing each closed neighbor
        revealAll (toUnfoldable $ filter (\j -> isClosed $ unsafePartial $ fromJust $ field.cells !! j) neighbors) field
      else
        -- if the number of nearby flags is not equal to the number of nearby mines, do nothing
        Ok field

  -- this should be impossible. TODO: is our model off?
  Just { player: Open, underlying: Mine } -> Ok field
