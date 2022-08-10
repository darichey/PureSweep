module Minesweeper.Eval where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST)
import Control.Monad.ST.Ref as STRef
import Data.Array.ST as STArray
import Data.HashSet as HashSet
import Data.Maybe (fromJust)
import Minesweeper.Model (GameOverKind, PlayerState(..), STField, UnderlyingCellState(..))
import Minesweeper.Monad (MinesweeperF(..))
import Partial.Unsafe (unsafePartial)

runMinesweeperF :: forall r. STField r -> MinesweeperF ~> ExceptT GameOverKind (ST r)
runMinesweeperF field = case _ of
  ModifyCell i f g -> do
    _ <- lift $ STArray.modify i f field.cells
    new <- lift $ STArray.peek i field.cells
    let cell = unsafePartial $ fromJust new

    case cell of
      { player: Open, underlying: Safe _ } -> void $ lift $ STRef.modify (HashSet.delete i) field.remainingSafe
      _ -> pure unit

    pure $ g $ cell
  GameOver x -> throwError x
  GetRemainingSafe f -> do
    remainingSafe <- lift $ STRef.read field.remainingSafe
    pure $ f remainingSafe
  GetMineIndices f -> pure $ f field.mineIndices
  GetDims f -> pure $ f field.dims
