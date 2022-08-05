module App (appComponent) where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (foldFree)
import Control.Monad.ST.Class (liftST)
import Data.Array (mapWithIndex)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Minesweeper.Eval (runMinesweeperF)
import Minesweeper.Model (Field, PlayerState(..), CellIndex, makeRandomField)
import Minesweeper.Monad (chordAt, revealAt, toggleFlagAt)
import OnContextMenu (onContextMenu)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.HTML.Common (PropName(..))
import Web.UIEvent.MouseEvent (button)

_field = Proxy :: Proxy "field"

_optionDial = Proxy :: Proxy "optionDial"

appComponent :: forall query input output m. MonadEffect m => H.Component query input output m
appComponent =
  Hooks.component \_ _ -> Hooks.do
    width /\ widthId <- Hooks.useState 10
    height /\ heightId <- Hooks.useState 10
    mines /\ minesId <- Hooks.useState 10
    fieldState /\ fieldId <- Hooks.useState Nothing
    Hooks.captures { width, height, mines } Hooks.useTickEffect do
      newField <- liftEffect $ makeRandomField width height mines
      Hooks.put fieldId (Just newField)
      pure Nothing
    Hooks.pure
      $ HH.div_
          [ HH.p_ [ HH.text "Options" ]
          , HH.slot _optionDial 0 optionDialComponent
              { name: "Width", num: width, min: Just 1, max: Nothing }
              ( \w -> do
                  Hooks.modify_ minesId (min (w * height))
                  Hooks.put widthId w
              )
          , HH.slot _optionDial 1 optionDialComponent
              { name: "Height", num: height, min: Just 1, max: Nothing }
              ( \h -> do
                  Hooks.modify_ minesId (min (width * h))
                  Hooks.put heightId h
              )
          , HH.slot _optionDial 2 optionDialComponent
              { name: "Mines", num: mines, min: Just 0, max: Just (width * height) }
              (Hooks.put minesId)
          , HH.p_ [ HH.text "Game" ]
          , case fieldState of
              Nothing -> HH.div_ [ HH.text "loading" ]
              Just field -> HH.div_ [ HH.slot _field 3 fieldComponent field (handleFieldUpdate field fieldId) ]
          ]
  where
  handleFieldUpdate field fieldId playerAction = do
    let
      gameAction = case playerAction of
        RevealAt i -> revealAt i
        ChordAt i -> chordAt i
        FlagAt i -> toggleFlagAt i
    cells <- liftEffect $ liftST $ STArray.unsafeThaw field.cells
    result <- liftEffect $ liftST $ runExceptT $ foldFree (runMinesweeperF { cells, dims: field.dims }) gameAction
    newCells <- liftEffect $ liftST $ STArray.unsafeFreeze cells
    case result of
      Left _ -> pure unit
      Right _ -> Hooks.put fieldId (Just field { cells = newCells })

data PlayerAction = RevealAt CellIndex | ChordAt CellIndex | FlagAt CellIndex

fieldComponent :: forall query m. MonadEffect m => H.Component query Field PlayerAction m
fieldComponent =
  Hooks.component \{ outputToken } field -> Hooks.do
    Hooks.pure
      $ HH.div_
          [ HH.div
              [ twclass "inline-grid gap-1 select-none"
              , HP.style $ "grid-template-columns: repeat(" <> show field.dims.width <> ", minmax(0, 1fr))"
              , HP.style $ "grid-template-rows: repeat(" <> show field.dims.height <> ", minmax(0, 1fr))"
              , HP.draggable false
              ]
              $ mapWithIndex
                  ( \i cell ->
                      HH.div
                        [ twclass "flex text-center justify-center content-center select-none"
                        , HP.draggable false
                        , onContextMenu \event -> liftEffect $ preventDefault event
                        , HE.onMouseDown \event -> case button event of
                            0 -> Hooks.raise outputToken (RevealAt i)
                            1 -> Hooks.raise outputToken (ChordAt i)
                            2 -> Hooks.raise outputToken (FlagAt i)
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

type OptionDialInput = { name :: String, num :: Int, min :: Maybe Int, max :: Maybe Int }

optionDialComponent :: forall query m. H.Component query OptionDialInput Int m
optionDialComponent =
  Hooks.component \{ outputToken } { name, num, min, max } -> Hooks.do
    Hooks.pure
      $ HH.div_
          [ HH.button [ HE.onClick \_ -> Hooks.raise outputToken (clampMaybe min max (num + 1)) ] [ HH.text "+" ]
          , HH.button [ HE.onClick \_ -> Hooks.raise outputToken (clampMaybe min max (num - 1)) ] [ HH.text "-" ]
          , HH.text $ name <> ": " <> show num
          ]

clampMaybe :: Maybe Int -> Maybe Int -> Int -> Int
clampMaybe low hi x = clamp (fromMaybe bottom low) (fromMaybe top hi) x

twclass :: forall r i. String -> HP.IProp (class :: String | r) i
twclass = HP.prop (PropName "className")
