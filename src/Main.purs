module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Model (debugField, makeRandomField)

main :: Effect Unit
main = do
  field <- makeRandomField 10 10 10
  log $ debugField field
