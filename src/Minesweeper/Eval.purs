module Minesweeper.Eval where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST)
import Data.Array.ST as STArray
import Data.Maybe (fromJust)
import Minesweeper.Monad (MinesweeperF(..))
import Minesweeper.Model (Field)
import Partial.Unsafe (unsafePartial)

data Explosion = Explosion

runMinesweeperF :: forall r. Field r -> MinesweeperF ~> ExceptT Explosion (ST r)
runMinesweeperF field = case _ of
  ModifyCell i f g -> do
    _ <- lift $ STArray.modify i f field.cells
    new <- lift $ STArray.peek i field.cells
    pure $ g $ unsafePartial $ fromJust new
  Explode -> throwError Explosion
  GetDims f -> pure $ f field.dims