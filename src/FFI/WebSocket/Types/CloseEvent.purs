module FFI.WebSocket.Types.CloseEvent
  ( CloseEvent
  , CloseCode(..)
  , fromEvent
  , codeMatches
  , toCodeAndReason
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Web.Event.Event (Event)

type CloseEvent = { reason :: String, code :: Int }

foreign import _fromEventImpl :: Fn1 Event (Nullable CloseEvent)

fromEvent :: Event -> Maybe CloseEvent
fromEvent evt = toMaybe $ runFn1 _fromEventImpl evt

data CloseCode
  = NormalClosure
  | NoLongerInterested

toCodeAndReason :: CloseCode -> Tuple Int String
toCodeAndReason = case _ of
  NormalClosure -> Tuple 1000 "Normal closure"
  NoLongerInterested -> Tuple 3000 "No longer interested"

codeMatches :: CloseCode -> CloseEvent -> Boolean
codeMatches closeCode closeEvent =
  code == closeEvent.code
  where
  Tuple code _ = toCodeAndReason closeCode
