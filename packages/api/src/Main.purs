module Api.Main where

import HTTPurple
import Prelude hiding ((/))

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Debug as Debug
import Effect (Effect)
import Effect.Aff.Compat
  ( EffectFn1
  , EffectFn3
  , mkEffectFn1
  , mkEffectFn3
  , runEffectFn1
  )
import Effect.Exception (Error)
import Effect.Exception.Unsafe (unsafeThrow)
import Literals.Undefined (undefined)
import Node.Encoding as Encoding
import Node.HTTP.IncomingMessage as NodeIM
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.ServerResponse as ServerResponse
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Node.Stream as Writable
import Untagged.Union (UndefinedOr, asOneOf)

data Route
  = Api String
  | WS

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Api": "api" / segment
  , "WS": "ws" / noArgs
  }

handleWS :: RequestHeaders -> ResponseM
handleWS reqHeaders =
  case lookup reqHeaders wsKeyKey of
    Nothing -> badRequest $ "missing websocket key header: " <> wsKeyKey
    Just wsKey ->
      switchingProtocols' $ headers
        { "Sec-WebSocket-Accept": wsKey }
  where
  wsKeyKey = "Sec-WebSocket-Key"

type PursMiddleware =
  (IncomingMessage IMServer)
  -> ServerResponse
  -> (UndefinedOr Error -> Effect Unit)
  -> Effect Unit

pursMiddleware
  :: PursMiddleware
  -> NodeMiddleware ()
pursMiddleware middleware = NodeMiddleware $ mkEffectFn3 help
  where
  next' n = runEffectFn1 n
  help request response next = do
    middleware request response $ next' next
    pure $ pure unit

parseUrlParts :: String -> Array String
parseUrlParts =
  String.split (String.Pattern "/")
    >>> Array.filter (not <<< String.null)

myMiddleware :: PursMiddleware
myMiddleware request response next = do
  let
    method = NodeIM.method request
    url = NodeIM.url request
    urlParts = parseUrlParts url
  Debug.traceM { method, url, urlParts }
  case method, urlParts of
    "GET", [ "ws" ] -> do
      socket <- NodeIM.socket request
      Debug.traceM { socket, method, url }
      ServerResponse.setStatusCode 200 response
      let
        payload = "Hello, world!"
        outMsg = ServerResponse.toOutgoingMessage response
      OutgoingMessage.appendHeader "Content-Type" "text/plain" outMsg
      OutgoingMessage.appendHeader
        "Content-Length"
        (show $ String.length payload)
        outMsg
      let
        writeable = OutgoingMessage.toWriteable outMsg
      void $ Writable.writeString writeable Encoding.UTF8 payload
    _, _ ->
      next $ asOneOf undefined

nodeMiddleware :: NodeMiddlewareStack () ()
nodeMiddleware =
  NodeMiddlewareStack
    $ usingMiddleware
    $ pursMiddleware myMiddleware

main :: ServerM
main = do
  serveNodeMiddleware { port: 8080 } { route, router, nodeMiddleware }
  where
  router = case _ of
    { route: Api rest } -> ok $ "api route " <> rest
    { route: WS, headers } -> handleWS headers

