module FFI.WebSocket.Types.CloseEvent
  ( CloseEvent
  , CloseCode(..)
  , fromEvent
  , toCode
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Web.Event.Event (Event)

type CloseEvent = { reason :: String, code :: Int }

foreign import _fromEventImpl :: Fn1 Event (Nullable CloseEvent)

fromEvent :: Event -> Maybe CloseEvent
fromEvent evt = toMaybe $ runFn1 _fromEventImpl evt

data CloseCode
  = NormalClosure
  | GoingAway
  | AbnormalClosure

toCode :: CloseCode -> Int
toCode = case _ of
  NormalClosure -> 1000
  GoingAway -> 1001
  AbnormalClosure -> 1006
