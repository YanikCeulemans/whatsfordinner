module Data.Route
  ( Route(..)
  , GroceryListRoute
  , GroceryListInnerRoute(..)
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
import FFI.URL as URL
import Routing.Duplex (RouteDuplex', root, (:=))
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Type.Prelude (Proxy(..))

groceryListId :: RouteDuplex' String -> RouteDuplex' GroceryListId
groceryListId =
  D.as Id.print Id.parse

data GroceryListInnerRoute
  = Groceries
  | GroceriesGenerate
  | AddGrocery

derive instance Generic GroceryListInnerRoute _
derive instance Eq GroceryListInnerRoute

type GroceryListRoute =
  { groceryListId :: GroceryListId
  , groceryListRoute :: GroceryListInnerRoute
  }

data Route
  = Home
  | Schedule
  | GroceryListRoute GroceryListRoute

derive instance Generic Route _
derive instance Eq Route

groceryListInnerRoute :: RouteDuplex' GroceryListInnerRoute
groceryListInnerRoute =
  sum
    { "Groceries": "groceries" / noArgs
    , "GroceriesGenerate": "groceries" / "generate" / noArgs
    , "AddGrocery": "groceries" / "add" / noArgs
    }

groceryListRoute :: RouteDuplex' GroceryListRoute
groceryListRoute =
  D.record
    # (Proxy :: _ "groceryListId") := (groceryListId D.segment)
    # (Proxy :: _ "groceryListRoute") := groceryListInnerRoute

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "Schedule": "schedule" / noArgs
  , "GroceryListRoute": groceryListRoute
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

