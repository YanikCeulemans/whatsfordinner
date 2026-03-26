module Data.Route (Route(..), parse, print) where

import Prelude hiding ((/))

import Data.Array (fold)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import FFI.URL as URL
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Groceries
  | GroceriesGenerate

derive instance Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "Groceries": "groceries" / noArgs
  , "GroceriesGenerate": "groceries" / "generate" / noArgs
  }

parseRouteFromPathAndQuery :: String -> Maybe Route
parseRouteFromPathAndQuery = D.parse route >>> Either.hush

parse :: String -> Maybe Route
parse urlText = do
  url <- Either.hush $ URL.mk urlText
  parseRouteFromPathAndQuery $ fold [ URL.pathname url, URL.search url ]

print :: Route -> String
print = D.print route

