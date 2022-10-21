module Router.Route (Route(..), matchRoute) where

import Prelude

import Data.Foldable (oneOf)
import Routing.Match (Match, end, lit)

data Route = Game | Editor

derive instance Eq Route

matchRoute :: Match Route
matchRoute = oneOf [Editor <$ lit "editor", pure Game] <* end
