module Api.WS
  ( Error
  , WebSocket
  , WebSocketServer
  , WebSocketServerOptions
  , MessageData
  , mkWebSocketServer
  , connectionH
  , emitConnection
  , handleUpgrade
  , errorH
  , messageH
  , send
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , EffectFn5
  , mkEffectFn1
  , mkEffectFn2
  , runEffectFn1
  , runEffectFn2
  , runEffectFn3
  , runEffectFn5
  )
import Node.Buffer (Buffer)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle1, EventHandle2)
import Node.HTTP.Types (IMServer, IncomingMessage)
import Node.Net.Types (Socket, TCP)
import Untagged.Union (type (|+|))

data Error

data WebSocket

data WebSocketServer

type WebSocketServerOptions = { noServer :: Boolean }

foreign import _mkWebSocketServerImpl
  :: EffectFn1 WebSocketServerOptions WebSocketServer

mkWebSocketServer :: WebSocketServerOptions -> Effect WebSocketServer
mkWebSocketServer = runEffectFn1 _mkWebSocketServerImpl

connectionH :: EventHandle1 WebSocketServer WebSocket
connectionH = EventHandle "connection" mkEffectFn1

foreign import _emitConnectionImpl
  :: EffectFn3 WebSocket (IncomingMessage IMServer) WebSocketServer Unit

emitConnection
  :: WebSocket -> IncomingMessage IMServer -> WebSocketServer -> Effect Unit
emitConnection = runEffectFn3 _emitConnectionImpl

foreign import _handleUpgradeImpl
  :: EffectFn5 (IncomingMessage IMServer) (Socket TCP) Buffer
       (EffectFn1 WebSocket Unit)
       WebSocketServer
       Unit

handleUpgrade
  :: (IncomingMessage IMServer)
  -> Socket TCP
  -> Buffer
  -> (WebSocket -> Effect Unit)
  -> WebSocketServer
  -> Effect Unit
handleUpgrade request socket buffer cb wss =
  runEffectFn5 _handleUpgradeImpl request socket buffer adaptedCb wss
  where
  adaptedCb = mkEffectFn1 cb

errorH :: EventHandle1 WebSocket Error
errorH = EventHandle "error" mkEffectFn1

type MessageData = ArrayBuffer |+| Buffer |+| Array Buffer

messageH :: EventHandle2 WebSocket MessageData Boolean
messageH = EventHandle "message" mkEffectFn2

foreign import _sendImpl :: EffectFn2 String WebSocket Unit

send :: String -> WebSocket -> Effect Unit
send = runEffectFn2 _sendImpl

