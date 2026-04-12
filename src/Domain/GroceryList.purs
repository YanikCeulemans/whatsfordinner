module Domain.GroceryList where

import Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Maybe as Maybe
import Domain.Grocery (Grocery)
import Domain.Grocery as Grocery
import Domain.GroceryId (GroceryId)

type GroceryList = Array Grocery

codec :: CA.JsonCodec GroceryList
codec =
  CA.array Grocery.codec

upsertGrocery :: Grocery -> GroceryList -> GroceryList
upsertGrocery grocery groceryList =
  {--  
    TODO: what is the proper identity of a grocery? 
    An arbitrary ULID id field doesn't seem to be the answer. What is the
    desired behaviour of adding multiple groceries to the grocery list?
      - Do we want the possiblity to have multiple lines with the same description
      - What happens when someone enters Carrots x5 when there is already Carrots 1kg on the list?
  --}
  (Array.findIndex hasId groceryList >>= update)
    # Maybe.fromMaybe' consGroceryList
  where
  hasId x = Grocery.id x == Grocery.id grocery
  update i = Array.updateAt i grocery groceryList
  consGroceryList _ = Array.cons grocery groceryList

toggleGrocery :: GroceryId -> GroceryList -> GroceryList
toggleGrocery id groceryList =
  groceryList <#> toggle
  where
  toggle grocery
    | Grocery.id grocery == id = Grocery.toggleChecked grocery
    | otherwise = grocery

deleteGroceries :: Array Grocery -> GroceryList -> GroceryList
deleteGroceries groceries groceryList =
  Array.difference groceryList groceries
