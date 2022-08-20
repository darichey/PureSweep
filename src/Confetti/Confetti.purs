module Confetti where

import Prelude

import Effect (Effect)

foreign import doWinConfetti :: Effect Unit
