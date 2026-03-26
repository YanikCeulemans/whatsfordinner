module App.Shared where

import Prelude

import Data.Route (Route)
import Data.Route as Route
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

link :: forall msg. Route -> Array (Html msg) -> Html msg
link route content = HE.a [ HA.href $ Route.print route ] content
