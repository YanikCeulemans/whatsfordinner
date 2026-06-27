module FFI.WebSocket
  ( WebSocket
  , mk
  , close
  , toEventTarget
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Unsafe.Coerce (unsafeCoerce)
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
