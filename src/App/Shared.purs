module App.Shared where

import Prelude

import Data.Array as Array
import Data.Route (Route)
import Data.Route as Route
import Data.Tuple (Tuple, fst, snd)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Row.Homogeneous (class Homogeneous)

link :: forall w i. Route -> Array (HH.HTML w i) -> HH.HTML w i
link route content = HH.a [ HP.href $ Route.print route ] content

classes
  :: forall r i
   . Array (Tuple String Boolean)
  -> HP.IProp (class :: String | r) i
classes cs =
  cs # Array.filter snd # map (fst >>> H.ClassName) # HP.classes

classes'
  :: forall r r1 i
   . Homogeneous r Boolean
  => Record r
  -> HP.IProp (class :: String | r1) i
classes' r =
  FO.fromHomogeneous r
    # FO.filter identity
    # FO.keys
    # map H.ClassName
    # HP.classes

