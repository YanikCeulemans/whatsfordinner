module Api.Main where

import HTTPurple
import Prelude hiding ((/))

data Route = Api String

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Api": "api" / segment
  }

main :: ServerM
main = do
  serve { port: 8080 } { route, router }
  where
  router { route: Api rest } = ok $ "api route " <> rest

