module Data.Route
  ( Route(..)
  , SpaceRoute
  , SpaceInnerRoute(..)
  , parse
  , parse'
  , print
  ) where

import Prelude hiding ((/))

import Data.Array (fold)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Domain.Id as Id
import Domain.SpaceId (SpaceId)
import FFI.URL as URL
import Routing.Duplex (RouteDuplex', root, (:=))
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Type.Prelude (Proxy(..))

spaceId :: RouteDuplex' String -> RouteDuplex' SpaceId
spaceId =
  D.as Id.print Id.parse

data SpaceInnerRoute
  = Schedule
  | Groceries
  | GroceriesGenerate
  | AddGrocery

derive instance Generic SpaceInnerRoute _
derive instance Eq SpaceInnerRoute

type SpaceRoute =
  { spaceId :: SpaceId
  , route :: SpaceInnerRoute
  }

data Route
  = Home
  | SpaceRoute SpaceRoute

derive instance Generic Route _
derive instance Eq Route

spaceInnerRoute :: RouteDuplex' SpaceInnerRoute
spaceInnerRoute =
  sum
    { "Schedule": "schedule" / noArgs
    , "Groceries": "groceries" / noArgs
    , "GroceriesGenerate": "groceries" / "generate" / noArgs
    , "AddGrocery": "groceries" / "add" / noArgs
    }

spaceRoute :: RouteDuplex' SpaceRoute
spaceRoute =
  D.record
    # (Proxy :: _ "spaceId") := (spaceId D.segment)
    # (Proxy :: _ "route") := spaceInnerRoute

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "SpaceRoute": spaceRoute
  }

parseRouteFromPathAndQuery :: String -> Maybe Route
parseRouteFromPathAndQuery = D.parse route >>> Either.hush

parse :: String -> Maybe Route
parse urlText = do
  url <- Either.hush $ URL.mk urlText
  parseRouteFromPathAndQuery $ fold [ URL.pathname url, URL.search url ]

parse' :: String -> Maybe Route
parse' = parseRouteFromPathAndQuery

print :: Route -> String
print = D.print route

