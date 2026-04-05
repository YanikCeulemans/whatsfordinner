module App.Shared where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Route (Route)
import Data.Route as Route
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Effect.Class (class MonadEffect)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Row.Homogeneous (class Homogeneous)
import Web.Event.Event (Event)
import Web.Event.Event as E
import Web.Event.Event as Event
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

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

eventTargetInputValue :: forall m. MonadEffect m => Event -> m (Maybe String)
eventTargetInputValue event =
  Event.target event
    >>= InputElement.fromEventTarget
    # traverse InputElement.value
    # H.liftEffect

eventTargetInputValueOrEmpty :: forall m. MonadEffect m => Event -> m String
eventTargetInputValueOrEmpty event =
  Event.target event
    >>= InputElement.fromEventTarget
    # traverse InputElement.value
    <#> Maybe.fromMaybe ""
    # H.liftEffect

class PreventableEvent e where
  preventDefault :: forall m. MonadEffect m => e -> m Unit

instance PreventableEvent Event where
  preventDefault = H.liftEffect <<< E.preventDefault

instance PreventableEvent MouseEvent where
  preventDefault = preventDefault <<< MouseEvent.toEvent

instance PreventableEvent DragEvent where
  preventDefault = preventDefault <<< DragEvent.toEvent

