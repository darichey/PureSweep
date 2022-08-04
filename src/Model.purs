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
  , toggleFlagAt
  , revealAt
  ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Control.Monad.ST (Region, ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Data.Array (catMaybes, filter, foldr, fromFoldable, length, replicate, toUnfoldable, (!!), (..))
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, maybe)
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

type Field :: Region -> Type
type Field h = { cells :: STArray h Cell, width :: Width, height :: Height }

makeField :: forall h. Width -> Height -> HashSet CellIndex -> ST h (Field h)
makeField width height mineIndices = do
  cells <- STArray.unsafeThaw $ replicate (width * height) { underlying: Safe 0, player: Closed }
  ST.foreach (HashSet.toArray mineIndices)
    ( \i ->
        do
          void $ STArray.poke i { underlying: Mine, player: Closed } cells
          ST.foreach (getNeighbors i width height) (\j -> void $ STArray.modify j incNearby cells)
    )
  pure { cells, width, height }

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

makeRandomField :: Width -> Height -> Int -> Effect (Field Global)
makeRandomField width height numMines = do
  mineIndices <- sample (0 .. (width * height - 1)) numMines
  field <- liftST $ makeField width height mineIndices
  pure field

data RevealResult = Ok | Explode

revealAt :: forall h. CellIndex -> (Field h) -> ST h RevealResult
revealAt i field = revealWithZeroProp [ i ] field

revealWithZeroProp :: forall h. Array CellIndex -> (Field h) -> ST h RevealResult
revealWithZeroProp indices field = do
  cellsView <- STArray.unsafeFreeze field.cells
  let zeroPropagated = go cellsView (toUnfoldable indices) HashSet.empty Nil
  revealAll (fromFoldable zeroPropagated) field
  where

  go :: Array Cell -> List CellIndex -> HashSet CellIndex -> List CellIndex -> List CellIndex
  go _ Nil _ acc = acc
  go cellsView (next : stack) visited acc =
    if HashSet.member next visited then go cellsView stack visited acc
    else
      case cellsView !! next of
        Just { underlying: Safe 0 } -> go cellsView (pushAll (getNeighbors next field.width field.height) stack) (HashSet.insert next visited) (next : acc)
        Just { underlying: Safe _ } -> go cellsView stack (HashSet.insert next visited) (next : acc)
        _ -> go cellsView stack visited acc

  pushAll :: Array CellIndex -> List CellIndex -> List CellIndex
  pushAll array list = foldr (:) list array

revealAll :: forall h. Array CellIndex -> (Field h) -> ST h RevealResult
revealAll indices field = do
  result <- runMaybeT
    $ void
    $ sequence
    $ indices <#> \i -> do
        cell <- lift $ STArray.peek i field.cells
        case cell of
          Nothing -> pure unit
          Just { player: Open } -> pure unit
          Just { player: Flag } -> pure unit
          Just { underlying: Safe _ } -> lift $ void $ STArray.modify i makeOpen field.cells
          Just { underlying: Mine } -> MaybeT (pure Nothing)
  pure $ maybe Explode (const Ok) result

toggleFlagAt :: forall h. CellIndex -> (Field h) -> ST h Boolean
toggleFlagAt i field = STArray.modify i toggleFlag field.cells

chordAt :: forall h. CellIndex -> (Field h) -> ST h RevealResult
chordAt i field = do
  cellsView <- STArray.unsafeFreeze field.cells
  case cellsView !! i of
    -- if it was out of bounds somehow, closed, or a flag, just no-op
    Nothing -> pure Ok
    Just { player: Flag } -> pure Ok
    Just { player: Closed } -> pure Ok

    -- if it's safe, try to chord there
    Just { player: Open, underlying: Safe nearby } ->
      let
        neighbors = getNeighbors i field.width field.height
        numNearbyFlags = length $ filter (\j -> isFlag $ unsafePartial $ fromJust $ cellsView !! j) neighbors
      in
        if numNearbyFlags == nearby then
          -- do the chord by revealing each closed neighbor
          revealWithZeroProp (toUnfoldable $ filter (\j -> isClosed $ unsafePartial $ fromJust $ cellsView !! j) neighbors) field
        else
          -- if the number of nearby flags is not equal to the number of nearby mines, do nothing
          pure Ok

    -- this should be impossible. TODO: is our model off?
    Just { player: Open, underlying: Mine } -> pure Ok
