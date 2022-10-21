module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect, mkTell)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router (Query(..), routerComponent)
import Router.Route (matchRoute)
import Routing.Hash (matches)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- runUI routerComponent unit body

    void $ liftEffect $ matches matchRoute \old new -> do
      when (old /= Just new) $ launchAff_ do
        _response <- halogenIO.query $ mkTell $ Navigate new
        pure unit
