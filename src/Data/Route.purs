module Data.Route
  ( Route(..)
  , SpaceRoute(..)
  , GroceriesRoute(..)
  , parse
  , parse'
  , print
  ) where

import Prelude hiding ((/))

import Data.Array (fold)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Domain.GroceryListId (GroceryListId)
import Domain.Id as Id
import Domain.SpaceId (SpaceId)
import FFI.URL as URL
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

spaceId :: RouteDuplex' String -> RouteDuplex' SpaceId
spaceId =
  D.as Id.print Id.parse

groceryListId :: RouteDuplex' String -> RouteDuplex' GroceryListId
groceryListId =
  D.as Id.print Id.parse

data GroceriesRoute
  = Groceries
  | GroceriesGenerate
  | AddGrocery

derive instance Generic GroceriesRoute _
derive instance Eq GroceriesRoute

groceriesRoute :: RouteDuplex' GroceriesRoute
groceriesRoute =
  sum
    { "Groceries": noArgs
    , "GroceriesGenerate": "generate" / noArgs
    , "AddGrocery": "add" / noArgs
    }

data SpaceRoute
  = Schedule
  | GroceriesRoute GroceryListId GroceriesRoute

derive instance Generic SpaceRoute _
derive instance Eq SpaceRoute

data Route
  = Home
  | SpaceRoute SpaceId SpaceRoute

derive instance Generic Route _
derive instance Eq Route

spaceRoute :: RouteDuplex' SpaceRoute
spaceRoute =
  sum
    { "Schedule": "schedule" / noArgs
    , "Groceries": "groceries" / noArgs
    , "GroceriesRoute": "groceries" / groceryListId D.segment / groceriesRoute
    }

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "SpaceRoute": spaceId D.segment / spaceRoute
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

