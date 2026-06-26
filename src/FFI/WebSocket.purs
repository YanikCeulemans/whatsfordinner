module FFI.WebSocket
  ( WebSocket
  , WebSocketEvent(..)
  , mk
  , close
  , toEventTarget
  , toEventType
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import FFI.WebSocket.Types.CloseEvent (CloseCode, toCode)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventTarget)

data WebSocket

foreign import _mkImpl :: EffectFn1 String WebSocket

mk :: String -> Effect WebSocket
mk = runEffectFn1 _mkImpl

foreign import _closeImpl :: EffectFn2 Int WebSocket Unit

close :: CloseCode -> WebSocket -> Effect Unit
close cc = runEffectFn2 _closeImpl $ toCode cc

toEventTarget :: WebSocket -> EventTarget
toEventTarget = unsafeCoerce

data WebSocketEvent
  = Message
  | Close

toEventType :: WebSocketEvent -> EventType
toEventType = case _ of
  Message -> EventType "message"
  Close -> EventType "close"

