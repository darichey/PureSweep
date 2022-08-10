module App (appComponent) where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (foldFree)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Ref as STRef
import Data.Array (mapWithIndex)
import Data.Array.ST as STArray
import Data.Either (Either(..))
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
import Minesweeper.Eval (runMinesweeperF)
import Minesweeper.Model (CellIndex, Field, GameOverKind(..), PlayerState(..), makeRandomField)
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
    width /\ widthId <- Hooks.useState 10
    height /\ heightId <- Hooks.useState 10
    mines /\ minesId <- Hooks.useState 10
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
          , HH.button [ HE.onClick \_ -> resetGame ] [ HH.text "New Game" ]
          , HH.div_ [ HH.text $ show time ]
          , HH.p_ [ HH.text "Game" ]
          , case field of
              Nothing -> HH.div_ [ HH.text "loading" ]
              Just field' -> HH.div_ [ HH.slot _field 3 fieldComponent field' (handleFieldUpdate field' fieldId gameState gameStateId startTimer pauseTimer) ]
          , HH.div_
              [ HH.text $
                  case gameState of
                    New -> "New"
                    Playing -> "Playing"
                    Done Win -> "Win"
                    Done Lose -> "Lose"
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

      cells <- liftEffect $ liftST $ STArray.unsafeThaw field.cells
      remainingSafe <- liftEffect $ liftST $ STRef.new field.remainingSafe
      result <- liftEffect $ liftST $ runExceptT $ foldFree (runMinesweeperF (field { cells = cells, remainingSafe = remainingSafe })) gameAction
      newCells <- liftEffect $ liftST $ STArray.unsafeFreeze cells
      newRemainingSafe <- liftEffect $ liftST $ STRef.read remainingSafe

      case result of
        Left kind -> do
          pauseTimer
          Hooks.put gameStateId (Done kind)
        Right _ -> Hooks.put fieldId (Just field { cells = newCells, remainingSafe = newRemainingSafe })

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
