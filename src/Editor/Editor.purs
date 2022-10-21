module Editor where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks

editorComponent :: forall query input output m. MonadAff m => H.Component query input output m
editorComponent =
    Hooks.component \_ _ -> Hooks.do
        Hooks.pure $ HH.div_ [ HH.text "Editor" ]
