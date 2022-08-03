module Sample (sample) where

import Prelude

import Control.Monad.Rec.Loops (unfoldrM, untilJust)
import Data.Array (length, (!!))
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Partial.Unsafe (unsafePartial)

sample :: forall a. Hashable a => Array a -> Int -> Effect (HashSet a)
sample from n =
  if length from < n then
    throw $ "Cannot select " <> show n <> " items from an array of length " <> show (length from)
  else
    HashSet.fromArray
      <$> unfoldrM
          ( \{ set, remaining } ->
              if remaining == 0 then
                pure Nothing
              else do
                -- repeatedly select an element from `from` until it's one we haven't included yet
                e <- untilJust (map (\e -> if HashSet.member e set then Nothing else Just e) (randomElem from))
                pure $ Just $ Tuple e { set: HashSet.insert e set, remaining: remaining - 1 }
          )
          { set: HashSet.empty, remaining: n }

randomElem :: forall a. Array a -> Effect a
randomElem from = do
  i <- randomInt 0 (length from - 1)
  -- this is safe because i is selected from [0, length from)
  pure $ unsafePartial $ fromJust $ from !! i
