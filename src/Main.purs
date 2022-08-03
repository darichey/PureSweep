-- module Main where
-- import Prelude
-- import Effect (Effect)
-- import Effect.Console (log)
-- import Model (debugField, makeRandomField)
-- main :: Effect Unit
-- main = do
--   field <- makeRandomField 10 10 10
--   log $ debugField field
module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Model (Cell(..), Field(..), makeRandomField)
import Web.HTML.Common (PropName(..))

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

makeFieldComponent :: forall query input output m. Field -> H.Component query input output m
makeFieldComponent (Field field) =
  Hooks.component \_ _ ->
    Hooks.pure do
      HH.div [ twclass "grid grid-cols-10" ]
        $ map
            ( \(Cell cell) ->
                HH.button [ twclass "text-center" ] [ HH.text $ show cell.underlying ]
            )
            field.cells
