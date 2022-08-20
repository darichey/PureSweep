module App (appComponent) where

import Prelude

import Confetti (doWinConfetti)
import Control.Monad.ST.Class (liftST)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Minesweeper.Eval (MinesweeperResult(..), runMinesweeperM)
import Minesweeper.Model (CellIndex, Field, GameOverKind(..), PlayerState(..), UnderlyingCellState(..), makeRandomField)
import Minesweeper.Monad (chordAt, revealAt, toggleFlagAt)
import OnContextMenu (onContextMenu)
import Type.Proxy (Proxy(..))
import UseTimer (useTimer)
import Web.Event.Event (preventDefault)
import Web.HTML.Common (PropName(..))
import Web.UIEvent.MouseEvent (button)

_field = Proxy :: Proxy "field"

_optionDial = Proxy :: Proxy "optionDial"

data PlayerAction = RevealAt CellIndex | ChordAt CellIndex | FlagAt CellIndex
data GameState = New | Playing | Done GameOverKind

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent =
  Hooks.component \_ _ -> Hooks.do
    width /\ widthId <- Hooks.useState 30
    height /\ heightId <- Hooks.useState 16
    mines /\ minesId <- Hooks.useState 99
    field /\ fieldId <- Hooks.useState Nothing
    { time, start: startTimer, pause: pauseTimer, reset: resetTimer } <- useTimer
    gameState /\ gameStateId <- Hooks.useState New

    let
      resetGame = do
        pauseTimer
        resetTimer
        newField <- liftEffect $ makeRandomField width height mines
        Hooks.put fieldId (Just newField)
        Hooks.put gameStateId New

    -- on first render, and any time width, height, or mines change, reset the game
    Hooks.captures { width, height, mines } Hooks.useTickEffect do
      resetGame
      pure Nothing -- no cleanup hook

    Hooks.pure
      $ HH.div
          [ twclass "flex flex-col gap-5" ]
          [ HH.div_
              [ HH.div
                  [ twclass "text-4xl text-center" ]
                  [ HH.a [ HP.href "https://darichey.github.io/PureSweep/" ] [ HH.text "PureSweep" ] ]
              , HH.div
                  [ twclass "text-xl text-gray-500 text-center" ]
                  [ HH.text "A Minesweeper clone written in "
                  , HH.a [ twclass "underline decoration-dotted", HP.href "https://www.purescript.org/" ] [ HH.text "PureScript" ]
                  ]
              ]
          , HH.div
              [ twclass "flex flex-row gap-1" ]
              [ HH.div
                  [ twclass "flex flex-col gap-1" ]
                  [ HH.div
                      [ twclass "inline-grid grid-cols-5 grid-rows-3 gap-1" ]
                      [ HH.slot _optionDial 0 optionDialComponent
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
                      ]
                  , HH.button
                      [ twclass "p-1 border-2 border-black rounded"
                      , HE.onClick \_ -> resetGame
                      ]
                      [ HH.text "New Game" ]
                  , HH.div_
                      [ HH.text $ "Time: " <> show time ]
                  ]
              , HH.div
                  [ twclass "flex-1 flex justify-center"
                  , HP.style $ "opacity: " <>
                      ( case gameState of
                          Done Lose -> "0.5"
                          _ -> "1"
                      )
                  ]
                  [ case field of
                      Nothing -> HH.text "Loading game..."
                      Just field' -> HH.slot _field 3 fieldComponent { field: field', gameState } (handleFieldUpdate field' fieldId gameState gameStateId startTimer pauseTimer)
                  ]
              ]
          ]
  where
  handleFieldUpdate :: Field -> StateId (Maybe Field) -> GameState -> StateId GameState -> HookM m Unit -> HookM m Unit -> PlayerAction -> HookM m Unit
  handleFieldUpdate field fieldId gameState gameStateId startTimer pauseTimer playerAction = do
    case gameState of
      New -> do
        startTimer
        Hooks.put gameStateId Playing
        doAction
      Playing -> doAction
      Done _ -> pure unit

    where
    doAction = do
      let
        gameAction = case playerAction of
          RevealAt i -> revealAt i
          ChordAt i -> chordAt i
          FlagAt i -> toggleFlagAt i

      result <- liftEffect $ liftST $ runMinesweeperM field gameAction

      case result of
        GameOver kind -> do
          pauseTimer
          case kind of
            Win -> liftEffect doWinConfetti
            Lose -> pure unit
          Hooks.put gameStateId (Done kind)
        Ok newField -> Hooks.put fieldId (Just newField)

fieldComponent :: forall query m. MonadEffect m => H.Component query { field :: Field, gameState :: GameState } PlayerAction m
fieldComponent =
  Hooks.component \{ outputToken } { field, gameState } -> Hooks.do
    Hooks.pure
      $ HH.div
          [ twclass "inline-grid select-none"
          , HP.style $ "grid-template-columns: repeat(" <> show field.dims.width <> ", minmax(0, 1fr))"
          , HP.draggable false
          ]
      $ mapWithIndex
          ( \i cell ->
              HH.div
                [ HP.draggable false
                , onContextMenu \event -> liftEffect $ preventDefault event
                , HE.onMouseDown \event -> case button event of
                    0 -> Hooks.raise outputToken (RevealAt i)
                    1 -> Hooks.raise outputToken (ChordAt i)
                    2 -> Hooks.raise outputToken (FlagAt i)
                    _ -> pure unit
                ]
                [ HH.img
                    [ twclass "select-none"
                    , HP.src $ "img/" <>
                        case cell of
                          { underlying: Mine, player: Open } -> "mine_explode.png"
                          { underlying: Mine, player: Closed } ->
                            case gameState of
                              Done _ -> "mine.png"
                              _ -> "closed.png"
                          { underlying: Mine, player: Flag } -> "flag.png"
                          { underlying: Safe n, player: Open } -> show n <> ".png"
                          { underlying: Safe _, player: Closed } -> "closed.png"
                          { underlying: Safe _, player: Flag } ->
                            case gameState of
                              Done _ -> "flag_wrong.png"
                              _ -> "flag.png"
                    , HP.width 64
                    , HP.draggable false
                    ]
                ]
          )
          field.cells

type OptionDialInput = { name :: String, num :: Int, min :: Maybe Int, max :: Maybe Int }

optionDialComponent :: forall query m. H.Component query OptionDialInput Int m
optionDialComponent =
  Hooks.component \{ outputToken } { name, num, min, max } -> Hooks.do
    Hooks.pure
      $ HH.div
          [ twclass "contents" ]
          [ HH.button
              [ twclass "col-span-1 p-1 border-2 border-black rounded text-red-500"
              , HE.onClick \_ -> Hooks.raise outputToken (clampMaybe min max (num - 1))
              ]
              [ HH.text "-" ]
          , HH.div
              [ twclass "col-span-2 p-1" ]
              [ HH.text $ name <> ":" ]
          , HH.div
              [ twclass "col-span-1 p-1 text-right" ]
              [ HH.text $ show num ]
          , HH.button
              [ twclass "col-span-1 p-1 border-2 border-black rounded text-green-500"
              , HE.onClick \_ -> Hooks.raise outputToken (clampMaybe min max (num + 1))
              ]
              [ HH.text "+" ]
          ]

clampMaybe :: Maybe Int -> Maybe Int -> Int -> Int
clampMaybe low hi x = clamp (fromMaybe bottom low) (fromMaybe top hi) x

twclass :: forall r i. String -> HP.IProp (class :: String | r) i
twclass = HP.prop (PropName "className")
