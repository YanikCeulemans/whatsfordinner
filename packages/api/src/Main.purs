module Api.Main where

import HTTPurple
import Prelude hiding ((/))

data Route
  = Api String
  | WS

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Api": "api" / segment
  , "WS": "ws" / noArgs
  }

main :: ServerM
main = do
  serve { port: 8080 } { route, router }
  where
  router = case _ of
    { route: Api rest } -> ok $ "api route " <> rest
    { route: WS, headers } -> switchingProtocols' $ headers
      { "Sec-WebSocket-Accept": lookup headers "Sec-WebSocket-Key" }

