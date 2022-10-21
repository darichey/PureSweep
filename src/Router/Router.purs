module Router where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Editor (editorComponent)
import Effect.Aff.Class (class MonadAff)
import Game (gameComponent)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Router.Route (Route(..), matchRoute)
import Routing.Hash (getHash, match)
import Type.Proxy (Proxy(..))

data Query a = Navigate Route a

_game = Proxy :: Proxy "game"

_editor = Proxy :: Proxy "editor"

routerComponent :: forall input output m. MonadAff m => H.Component Query input output m
routerComponent =
    Hooks.component \{ queryToken } _ -> Hooks.do
        route /\ routeId <- Hooks.useState Game

        Hooks.useLifecycleEffect do
          initialRoute <- hush <<< (match matchRoute) <$> liftEffect getHash
          Hooks.put routeId (fromMaybe Game initialRoute)
          pure Nothing

        Hooks.useQuery queryToken (case _ of
            Navigate dest a -> do
                Hooks.put routeId dest
                pure (Just a))

        Hooks.pure $ case route of
            Game -> HH.slot_ _game unit gameComponent unit
            Editor -> HH.slot_ _editor unit editorComponent unit
