module Api.Main where

import HTTPurple
import Prelude hiding ((/))

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug as Debug
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Compat (mkEffectFn3, runEffectFn1)
import Effect.Exception (Error)
import HTTPurple.Body (class Body)
import HTTPurple.Headers (mkRequestHeaders)
import Node.Buffer (Buffer)
import Node.Encoding as Encoding
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.ServerResponse as ServerResponse
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Node.Net.Types (Socket, TCP)
import Node.Stream (end')
import Node.Stream as Writable
import Untagged.Union (UndefinedOr)

data Route
  = Root
  | Api String

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Root": noArgs
  , "Api": "api" / segment
  }

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

httpStatusCodes
  :: { badRequest :: Int
     , ok :: Int
     , switchingProtocols :: Int
     }
httpStatusCodes =
  { switchingProtocols: 101
  , ok: 200
  , badRequest: 400
  }

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

onUpgrade :: IncomingMessage IMServer -> Socket TCP -> Buffer -> Effect Unit
onUpgrade request socket headBuffer = do
  Debug.traceM { request, socket, headBuffer }
  pure unit

main :: ServerM
main = do
  serve { port: 8080, onUpgrade: Just onUpgrade } { route, router }
  where
  router = case _ of
    { route: Root } -> ok rootView
    { route: Api rest } -> ok $ "api route " <> rest

