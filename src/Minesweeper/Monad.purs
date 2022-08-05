module Minesweeper.Monad
  ( MinesweeperF(..)
  , MinesweeperM
  , chordAt
  , toggleFlagAt
  , revealAt
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Array (filterA, fromFoldable, length, toUnfoldable)
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.List (List(..), foldr, sort, (:))
import Data.Traversable (sequence)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Minesweeper.Model (Cell, CellIndex, PlayerState(..), UnderlyingCellState(..), Dims, isClosed, isFlag, makeOpen, toggleFlag)
import Minesweeper.Model as Model

data MinesweeperF a
  = ModifyCell CellIndex (Cell -> Cell) (Cell -> a)
  | Explode
  | GetDims (Dims -> a)

derive instance Functor MinesweeperF

type MinesweeperM = Free MinesweeperF

get :: CellIndex -> MinesweeperM Cell
get i = liftF (ModifyCell i identity identity)

modify_ :: CellIndex -> (Cell -> Cell) -> MinesweeperM Unit
modify_ i f = void $ liftF (ModifyCell i f identity)

explode :: MinesweeperM Unit
explode = liftF Explode

getDims :: MinesweeperM Dims
getDims = liftF (GetDims identity)

ok :: MinesweeperM Unit
ok = pure unit

openCell :: CellIndex -> MinesweeperM Unit
openCell i = modify_ i makeOpen

getNeighbors :: CellIndex -> MinesweeperM (Array CellIndex)
getNeighbors index = do
  { width, height } <- getDims
  pure $ Model.getNeighbors index width height

revealAt :: CellIndex -> MinesweeperM Unit
revealAt i = revealWithZeroProp [ i ]

revealWithZeroProp :: Array CellIndex -> MinesweeperM Unit
revealWithZeroProp indices = do
  -- first, try to reveal the indices we're immediately interested in
  -- it's possible that one of these is a mine, and we're done
  revealAll indices
  -- assuming the above succeeded, now we can handle zero propagation
  -- the cells here must be safe, so we can reveal them without checking for game over
  -- FIXME: this needlessly tries to modify the cells corresponding to `indices` again
  zeroPropagated <- getZeroPropagated (toUnfoldable indices) HashSet.empty Nil
  revealAllUnchecked (fromFoldable zeroPropagated)

  where
  getZeroPropagated :: List CellIndex -> HashSet CellIndex -> List CellIndex -> MinesweeperM (List CellIndex)
  getZeroPropagated Nil _ acc = pure acc
  getZeroPropagated (next : stack) visited acc =
    if HashSet.member next visited then getZeroPropagated stack visited acc
    else
      do
        nextCell <- get next
        case nextCell of
          { underlying: Safe 0 } -> do
            neighbors <- getNeighbors next
            getZeroPropagated (pushAll neighbors stack) (HashSet.insert next visited) (next : acc)
          { underlying: Safe _ } -> getZeroPropagated stack (HashSet.insert next visited) (next : acc)
          _ -> getZeroPropagated stack visited acc
    where
    pushAll :: Array CellIndex -> List CellIndex -> List CellIndex
    pushAll array list = foldr (:) list array

revealAllUnchecked :: Array CellIndex -> MinesweeperM Unit
revealAllUnchecked indices = void $ sequence $ openCell <$> indices

revealAll :: Array CellIndex -> MinesweeperM Unit
revealAll indices = do
  void $ sequence $ indices <#> \i -> do
    cell <- get i
    case cell of
      { player: Open } -> ok
      { player: Flag } -> ok
      { underlying: Safe _ } -> openCell i
      { underlying: Mine } -> explode

toggleFlagAt :: CellIndex -> MinesweeperM Unit
toggleFlagAt i = modify_ i toggleFlag

chordAt :: CellIndex -> MinesweeperM Unit
chordAt i = do
  cell <- get i
  case cell of
    { player: Flag } -> ok
    { player: Closed } -> ok
    -- if it's safe, try to chord there
    { player: Open, underlying: Safe nearby } -> do
      neighbors <- getNeighbors i
      numNearbyFlags <- length <$> filterA (\j -> isFlag <$> get j) neighbors
      if numNearbyFlags == nearby then
        -- do the chord by revealing each closed neighbor
        do
          nearbyClosed <- filterA (\j -> isClosed <$> get j) neighbors
          revealWithZeroProp nearbyClosed
      else
        -- if the number of nearby flags is not equal to the number of nearby mines, do nothing
        ok
    -- this should be impossible. TODO: is our model off?
    { player: Open, underlying: Mine } -> ok
