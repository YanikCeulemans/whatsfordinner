module App.Shared where

import Prelude

import Data.Route (Route)
import Data.Route as Route
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

link :: forall w i. Route -> Array (HH.HTML w i) -> HH.HTML w i
link route content = HH.a [ HP.href $ Route.print route ] content
