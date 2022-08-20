module Minesweeper.Eval where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Except.Trans (lift, runExceptT)
import Control.Monad.Free (foldFree)
import Control.Monad.ST (ST)
import Control.Monad.ST.Ref as STRef
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.HashSet as HashSet
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Minesweeper.Model (Field, GameOverKind, PlayerState(..), STField, UnderlyingCellState(..))
import Minesweeper.Monad (MinesweeperF(..), MinesweeperM(..))
import Partial.Unsafe (unsafePartial)

data MinesweeperResult = GameOver GameOverKind | Ok Field

runMinesweeperF :: forall r. STField r -> MinesweeperF ~> ExceptT GameOverKind (ST r)
runMinesweeperF field = case _ of
  ModifyCell i f g -> do
    old' <- lift $ STArray.peek i field.cells
    _ <- lift $ STArray.modify i f field.cells
    new' <- lift $ STArray.peek i field.cells

    -- These are safe because we bounds check `i` earlier
    let old = unsafePartial $ fromJust old'
    let new = unsafePartial $ fromJust new'

    -- If the cell changed flag states (Flag -> Closed or vice versa), update the flag count
    case (old /\ new) of
      ({ player: Flag } /\ { player: Closed }) -> void $ lift $ STRef.modify (_ - 1) field.flags
      ({ player: Closed } /\ { player: Flag }) -> void $ lift $ STRef.modify (_ + 1) field.flags
      _ -> pure unit

    -- If the cell is now open and safe, remove it from the remainingSafe set
    -- Don't bother checking if it was previously closed, because HashSet.delete is idempotent
    case new of
      { player: Open, underlying: Safe _ } -> void $ lift $ STRef.modify (HashSet.delete i) field.remainingSafe
      _ -> pure unit

    pure $ g $ new
  EndGame x -> throwError x
  GetRemainingSafe f -> do
    remainingSafe <- lift $ STRef.read field.remainingSafe
    pure $ f remainingSafe
  GetMineIndices f -> pure $ f field.mineIndices
  GetDims f -> pure $ f field.dims

runMinesweeperM ∷ forall a r. Field -> MinesweeperM a -> ST r MinesweeperResult
runMinesweeperM field (MinesweeperM gameAction) = do
  cells <- STArray.unsafeThaw field.cells
  remainingSafe <- STRef.new field.remainingSafe
  flags <- STRef.new field.flags

  result <- runExceptT $ foldFree (runMinesweeperF (field { cells = cells, remainingSafe = remainingSafe, flags = flags })) gameAction

  newCells <- STArray.unsafeFreeze cells
  newRemainingSafe <- STRef.read remainingSafe
  newFlags <- STRef.read flags

  pure $ case result of
    Left kind -> GameOver kind
    Right _ -> Ok $ field { cells = newCells, remainingSafe = newRemainingSafe, flags = newFlags }
