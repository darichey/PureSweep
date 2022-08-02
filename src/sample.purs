module Sample (sample) where

import Prelude

import Data.Array (length, (!!))
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Random (randomInt)
import Partial.Unsafe (unsafePartial)

sample :: forall a. Hashable a => Array a -> Int -> Effect (HashSet a)
sample from n = go from n (HashSet.empty)

-- TODO: make this stack safe
go :: forall a. Hashable a => Array a -> Int -> HashSet a -> Effect (HashSet a)
go _ 0 acc = pure acc

go from n acc = do
  e <- randomElem from
  if HashSet.member e acc then
    go from n acc
  else
    go from (n - 1) (HashSet.insert e acc)

randomElem :: forall a. Array a -> Effect a
randomElem from = do
  i <- randomInt 0 (length from - 1)
  -- this is safe because i is selected from [0, length from)
  pure $ unsafePartial $ fromJust $ from !! i
