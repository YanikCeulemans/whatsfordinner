module FFI.WebSocket.Types.ReadyState
  ( ReadyState(..)
  , ParseReadyStateError(..)
  , fromInt
  ) where

import Prelude

import Data.Either (Either(..))

data ReadyState
  -- | Socket has been created. The connection is not yet open. (0)
  = Connecting
  -- | The connection is open and ready to communicate. (1)
  | Open
  -- | The connection is in the process of closing. (2)
  | Closing
  -- | The connection is closed or couldn't be opened. (3)
  | Closed

data ParseReadyStateError = UnknownReadyState Int

fromInt :: Int -> Either ParseReadyStateError ReadyState
fromInt = case _ of
  0 -> Right Connecting
  1 -> Right Open
  2 -> Right Closing
  3 -> Right Closed
  other -> Left $ UnknownReadyState other
