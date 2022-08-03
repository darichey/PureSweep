module OnContextMenu
  ( contextmenu
  , onContextMenu
  ) where

import Halogen.HTML.Events (handler)
import Halogen.HTML.Properties (IProp)
import Web.Event.Event (Event, EventType(..))

contextmenu :: EventType
contextmenu = EventType "contextmenu"

onContextMenu :: forall r i. (Event -> i) -> IProp ( onContextMenu :: Event | r ) i
onContextMenu = handler contextmenu
