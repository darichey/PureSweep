module Util.UseTimer
  ( UseTimer
  , useTimer
  ) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber, Milliseconds(..), error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, UseEffect, UseRef, UseState)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS

foreign import data UseTimer :: Hooks.HookType

type InternalState m = { listener :: Maybe (HS.Listener (HookM m Unit)), fiber :: Maybe (Fiber Unit) }

type UseTimer' m = UseState Int <> UseRef (InternalState m) <> UseEffect <> Hooks.Pure

instance HookNewtype UseTimer (UseTimer' m)

type Timer m = { time :: Int, pause :: HookM m Unit, reset :: HookM m Unit, start :: HookM m Unit }

useTimer :: forall m. MonadAff m => Hook m UseTimer (Timer m)
useTimer = Hooks.wrap hook
  where
  hook :: Hook m (UseTimer' m) (Timer m)
  hook = Hooks.do
    time /\ timeId <- Hooks.useState 0
    -- We don't care about the first value of the tuple, because we only use the ref values inside HookM.
    -- That means that we have to read from the ref manaully:
    -- > any effectful computations in HookM should read the value of the reference to guarantee an up-to-date value.
    _ /\ ref <- Hooks.useRef { listener: Nothing, fiber: Nothing }

    _ <- Hooks.useLifecycleEffect do
      { emitter, listener: listener' } <- H.liftEffect HS.create
      _ <- H.liftEffect $ Ref.modify_ (\state -> state { listener = Just listener' }) ref
      subscription <- Hooks.subscribe emitter
      pure $ Just $ Hooks.unsubscribe subscription

    let
      start = do
        { fiber: curFiber, listener: curListener } <- H.liftEffect $ Ref.read ref
        case curFiber of
          Just _ -> pure unit -- already started, no-op
          Nothing -> case curListener of
            Nothing -> pure unit -- this should be impossible
            Just listener' -> do
              fiber' <- H.liftAff $ Aff.forkAff $ forever do
                Aff.delay $ Milliseconds 1000.0
                H.liftEffect $ HS.notify listener' do
                  Hooks.modify_ timeId (_ + 1)
                  pure unit

              H.liftEffect $ Ref.modify_ (\state -> state { fiber = Just fiber' }) ref

      reset = Hooks.put timeId 0

      pause = do
        { fiber: curFiber } <- H.liftEffect $ Ref.read ref
        case curFiber of
          Nothing -> pure unit -- not running, no-op
          Just fiber' -> do
            _ <- H.liftAff $ Aff.killFiber (error "Timer Pause") fiber'
            H.liftEffect $ Ref.modify_ (\state -> state { fiber = Nothing }) ref

    Hooks.pure { time, start, pause, reset }
