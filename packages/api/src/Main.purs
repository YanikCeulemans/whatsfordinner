module Api.Main where

import HTTPurple
import Prelude hiding ((/))

import Api.Crypto as Crypto
import Control.Monad.Except (except, runExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Debug as Debug
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Compat (mkEffectFn3, runEffectFn1)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import HTTPurple.Body (class Body)
import HTTPurple.Headers (mkRequestHeaders)
import Literals.Undefined (undefined)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP.IncomingMessage as IncomingMessage
import Node.HTTP.IncomingMessage as NodeIM
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.ServerResponse as ServerResponse
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Node.Stream (end')
import Node.Stream as Writable
import Untagged.Union (UndefinedOr, asOneOf)

data Route
  = Root
  | Api String
  | WS

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Root": noArgs
  , "Api": "api" / segment
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

websocketKeyHeader :: String
websocketKeyHeader = "Sec-WebSocket-Key"

websocketAcceptHeader :: String
websocketAcceptHeader = "Sec-WebSocket-Accept"

switchingProtocols_ :: String -> ServerResponse -> Effect Unit
switchingProtocols_ _ _ = unsafeThrow "todo1"

badRequest_ :: ServerResponse -> Effect Unit
badRequest_ _ = unsafeThrow "todo2"

buildWsAcceptHeaderValue :: String -> String
buildWsAcceptHeaderValue key =
  Crypto.sha1 Encoding.Base64 $ key <> magicString
  where
  magicString = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

httpStatusCodes =
  { switchingProtocols: 101
  , ok: 200
  , badRequest: 400
  }

myMiddleware :: PursMiddleware
myMiddleware request response next = do
  let
    method = NodeIM.method request
    url = NodeIM.url request
    urlParts = parseUrlParts url
  Debug.traceM { method, url, urlParts }
  case method, urlParts of
    "GET", [ "ws" ] -> do
      result <- runExceptT do
        let
          headers = IncomingMessage.headers request
        websocketKey <- except
          $ Either.note ("missing header: " <> websocketKeyHeader)
          $ Object.lookup websocketKeyHeader headers

        let
          responseHeaders =
            [ Tuple "Upgrade" "websocket"
            , Tuple "Connection" "Upgrade"
            , Tuple websocketAcceptHeader $ buildWsAcceptHeaderValue
                websocketKey
            ]

        socket <- liftEffect $ NodeIM.socket request

        liftEffect $ ServerResponse.setStatusCode
          httpStatusCodes.switchingProtocols
          response

        let
          outMsg = ServerResponse.toOutgoingMessage response

        for_ responseHeaders \(Tuple key value) ->
          liftEffect $ OutgoingMessage.appendHeader key value outMsg

      case result of
        Left _e ->
          badRequest_ response
        Right _ ->
          pure unit
    _, _ ->
      next $ asOneOf undefined

nodeMiddleware :: NodeMiddlewareStack () ()
nodeMiddleware =
  NodeMiddlewareStack
    $ usingMiddleware
    $ pursMiddleware myMiddleware

data HTML
  = Node String (Array String) (Array HTML)
  | Content String

renderHTML :: HTML -> String
renderHTML html' = "<!DOCTYPE html>" <> help html'
  where
  help =
    case _ of
      Node tag attrs children ->
        Array.fold
          [ "<"
          , tag
          , case attrs of
              [] -> ""
              _ -> " " <> Array.intercalate " " attrs
          , ">"
          , Array.fold $ help <$> children
          , "</"
          , tag
          , ">"
          ]
      Content text' -> text'

instance Body HTML where
  -- TODO: This implementation renders the body twice, can we reduce it?
  -- - We could create a new type containing the rendered html that has an instance for Body
  defaultHeaders html' =
    pure $ mkRequestHeaders
      [ Tuple "Content-Type" "text/html"
      , Tuple "Content-Length" $ show $ String.length rendered
      ]
    where
    rendered = renderHTML html'
  write html' response = makeAff \done -> do
    let
      stream = OutgoingMessage.toWriteable $ ServerResponse.toOutgoingMessage
        response
    void
      $ Writable.writeString' stream Encoding.UTF8 rendered
      $ const
      $ end' stream
      $ const
      $ done
      $ Right unit
    pure nonCanceler
    where
    rendered = renderHTML html'

html :: Array String -> Array HTML -> HTML
html = Node "html"

head :: Array String -> Array HTML -> HTML
head = Node "head"

body :: Array String -> Array HTML -> HTML
body = Node "body"

button :: Array String -> Array HTML -> HTML
button = Node "button"

text :: String -> HTML
text = Content

script :: Array String -> Array HTML -> HTML
script = Node "script"

rootView :: HTML
rootView =
  html []
    [ head []
        [ script []
            [ text
                """
                let ws = null;
                const connect = () => {
                  if (ws) return;
                  ws = new WebSocket('/ws');
                  ws.addEventListener('message', evt => {
                    console.log('message received', evt);
                  });
                  ws.addEventListener('close', evt => {
                    console.log('socket closed', evt);
                  });
                };
                const disconnect = () => {
                  console.log("disconnect from js");
                  ws?.close();
                  ws = null;
                };
                """
            ]
        ]
    , body []
        [ button [ "onclick='connect()'" ] [ text "connect" ]
        , button [ "onclick='disconnect()'" ] [ text "disconnect" ]
        ]
    ]

main :: ServerM
main = do
  serveNodeMiddleware { port: 8080 } { route, router, nodeMiddleware }
  where
  router = case _ of
    { route: Root } -> ok rootView
    { route: Api rest } -> ok $ "api route " <> rest
    { route: WS, headers } -> handleWS headers

