module Spa.FFI.WebSocket
  ( WebSocket
  , WebSocketEvent(..)
  , EventConfig
  , mk
  , close
  , toEventTarget
  , toEventType
  , eventConfigs
  , readyState
  ) where

import Prelude

import Data.Array (fold)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Spa.FFI.WebSocket.Types.CloseEvent (CloseCode, toCodeAndReason)
import Spa.FFI.WebSocket.Types.CloseEvent as WSTC
import Spa.FFI.WebSocket.Types.MessageEvent as WSTM
import Spa.FFI.WebSocket.Types.ReadyState (ParseReadyStateError(..), ReadyState)
import Spa.FFI.WebSocket.Types.ReadyState as ReadyState
import Partial.Unsafe (unsafeCrashWith)
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

foreign import _readyStateImpl :: EffectFn1 WebSocket Int

readyState :: WebSocket -> Effect ReadyState
readyState ws =
  runEffectFn1 _readyStateImpl ws
    <#> ReadyState.fromInt
    <#> case _ of
      Left (UnknownReadyState code) ->
        unsafeCrashWith $ fold [ "unknown readyState code: ", show code ]
      Right parsedReadyState -> parsedReadyState
