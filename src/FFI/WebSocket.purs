module FFI.WebSocket
  ( WebSocket
  , Message
  , mk
  , close
  , toEventTarget
  , messageFromEvent
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget)

data WebSocket

foreign import _mkImpl :: EffectFn1 String WebSocket

mk :: String -> Effect WebSocket
mk = runEffectFn1 _mkImpl

foreign import _closeImpl :: EffectFn1 WebSocket Unit

close :: WebSocket -> Effect Unit
close = runEffectFn1 _closeImpl

toEventTarget :: WebSocket -> EventTarget
toEventTarget = unsafeCoerce

type Message = { data :: String }

foreign import _messageFromEventImpl :: Fn1 Event (Nullable Message)

messageFromEvent :: Event -> Maybe Message
messageFromEvent = toMaybe <<< runFn1 _messageFromEventImpl

