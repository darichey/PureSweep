module Split (chunksOf) where

import Prelude
import Data.Array (cons, drop, take)

build :: forall a. ((a -> Array a -> Array a) -> Array a -> Array a) -> Array a
build g = g (cons) []

chunksOf :: forall e. Int -> Array e -> Array (Array e)
chunksOf i ls = map (take i) (build (splitter ls))
  where
  splitter :: forall a. Array e -> (Array e -> a -> a) -> a -> a
  splitter [] _ n = n

  splitter l c n = l `c` splitter (drop i l) c n
