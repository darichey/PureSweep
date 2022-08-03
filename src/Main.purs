module Main where

import Prelude

import Data.Array (mapWithIndex, snoc)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Model (Field, PlayerState(..), RevealResult(..), isOpen, makeRandomField, revealAt, toggleFlagAt)
import OnContextMenu (onContextMenu)
import Web.Event.Event (preventDefault)
import Web.HTML.Common (PropName(..))
import Web.UIEvent.MouseEvent (button)
import Web.UIEvent.MouseEvent as MouseEvent

main :: Effect Unit
main = do
  field <- makeRandomField 10 10 10
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      component = makeFieldComponent field
    runUI component unit body

twclass :: forall r i. String -> HP.IProp ( class :: String | r ) i
twclass = HP.prop (PropName "className")

makeFieldComponent :: forall query input output m. MonadEffect m => Field -> H.Component query input output m
makeFieldComponent initialField =
  Hooks.component \_ _ -> Hooks.do
    field /\ fieldId <- Hooks.useState initialField
    Hooks.pure do
      HH.div_
        [ HH.div
            [ twclass "inline-grid grid-cols-10 gap-1 select-none"
            , HP.draggable false
            ]
            $ mapWithIndex
                ( \i cell ->
                    HH.div
                      [ twclass "flex text-center justify-center content-center select-none"
                      , HP.draggable false
                      , onContextMenu \event -> liftEffect $ preventDefault event
                      , HE.onMouseDown \event -> case button event of
                          0 -> case revealAt i field of
                            Ok newField -> Hooks.put fieldId newField
                            _ -> pure unit
                          -- 1 -> ?foo -- TODO: middle click
                          2 -> case toggleFlagAt i field of
                            Just newField -> Hooks.put fieldId newField
                            _ -> pure unit
                          _ -> pure unit
                      ]
                      [ HH.img
                          [ twclass "select-none"
                          , HP.src
                              $ case cell.player of
                                  Open -> "img/" <> show (cell.underlying) <> ".png"
                                  Closed -> "img/closed.png"
                                  Flag -> "img/flag.png"
                          , HP.width 64
                          , HP.draggable false
                          ]
                      ]
                )
                field.cells
        , HH.div_
            []
        ]

addClicked :: Int -> Array Int -> Array Int
addClicked i arr = snoc arr i
