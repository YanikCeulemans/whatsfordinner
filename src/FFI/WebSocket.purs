module FFI.WebSocket
  ( WebSocket
  , WebSocketEvent(..)
  , EventConfig
  , mk
  , close
  , toEventTarget
  , toEventType
  , eventConfigs
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import FFI.WebSocket.Types.CloseEvent (CloseCode, toCodeAndReason)
import FFI.WebSocket.Types.CloseEvent as WSTC
import FFI.WebSocket.Types.MessageEvent as WSTM
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as E
import Web.Event.EventTarget (EventTarget)

data WebSocket

foreign import _mkImpl :: EffectFn1 String WebSocket

mk :: String -> Effect WebSocket
mk = runEffectFn1 _mkImpl

foreign import _closeImpl :: EffectFn3 Int String WebSocket Unit

close :: CloseCode -> WebSocket -> Effect Unit
close cc = runEffectFn3 _closeImpl code reason
  where
  Tuple code reason = toCodeAndReason cc

toEventTarget :: WebSocket -> EventTarget
toEventTarget = unsafeCoerce

data WebSocketEvent
  = Message
  | Close

toEventType :: WebSocketEvent -> EventType
toEventType = case _ of
  Message -> EventType "message"
  Close -> EventType "close"

type EventConfig a =
  { fromEvent :: E.Event -> Maybe a
  , eventType :: EventType
  }

eventConfigs
  :: { message :: EventConfig WSTM.MessageEvent
     , close :: EventConfig WSTC.CloseEvent
     , open :: EventConfig Event
     }
eventConfigs =
  { message:
      { fromEvent: WSTM.fromEvent
      , eventType: EventType "message"
      }
  , close:
      { fromEvent: WSTC.fromEvent
      , eventType: EventType "close"
      }
  , open:
      { fromEvent: identity >>> Just
      , eventType: EventType "open"
      }
  }

