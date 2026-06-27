module FFI.WebSocket.Types.MessageEvent (MessageEvent, fromEvent) where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Web.Event.Event (Event)

type MessageEvent = { data :: String }

foreign import _messageEventFromEventImpl :: Fn1 Event (Nullable MessageEvent)

fromEvent :: Event -> Maybe MessageEvent
fromEvent = toMaybe <<< runFn1 _messageEventFromEventImpl

