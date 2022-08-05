module Minesweeper.Eval where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Data.Array.ST as STArray
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Minesweeper.Model (STField)
import Minesweeper.Monad (MinesweeperF(..))
import Partial.Unsafe (unsafePartial)

data Explosion = Explosion

runMinesweeperF :: forall r. STField r -> MinesweeperF ~> ExceptT Explosion (ST r)
runMinesweeperF field = case _ of
  ModifyCell i f g -> do
    _ <- lift $ STArray.modify i f field.cells
    new <- lift $ STArray.peek i field.cells
    pure $ g $ unsafePartial $ fromJust new
  Explode -> throwError Explosion
  GetDims f -> pure $ f field.dims

runMinesweeperFWithLogging :: STField Global -> MinesweeperF ~> ExceptT Explosion Effect
runMinesweeperFWithLogging field = case _ of
  ModifyCell i f g -> do
    old <- lift $ liftST $ STArray.peek i field.cells
    _ <- lift $ liftST $ STArray.modify i f field.cells
    new <- lift $ liftST $ STArray.peek i field.cells
    _ <- lift $ liftEffect $ Console.log $
      if old == new then
        "Get " <> show i
      else
        "Modify " <> show i <> " from " <> show old <> " to " <> show new
    pure $ g $ unsafePartial $ fromJust new
  Explode -> throwError Explosion
  GetDims f -> pure $ f field.dims
