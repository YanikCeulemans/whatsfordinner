module Data.Route (Route(..), parse, parse', print) where

import Prelude hiding ((/))

import Data.Array (fold)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.ULID as DULID
import FFI.URL as URL
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Simple.ULID (ULID)
import Simple.ULID as ULID

data Route
  = Home
  | Groceries
  | GroceriesGenerate
  | AddGrocery ULID

derive instance Generic Route _
derive instance Eq Route

ulid :: RouteDuplex' String -> RouteDuplex' ULID
ulid = as ULID.toString DULID.parse

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "Groceries": "groceries" / noArgs
  , "GroceriesGenerate": "groceries" / "generate" / noArgs
  , "AddGrocery": "groceries" / "add" / ulid segment
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

