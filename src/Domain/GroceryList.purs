module Domain.GroceryList where

import Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Maybe as Maybe
import Domain.GroceryEntry (GroceryEntry)
import Domain.GroceryEntry as GroceryEntry
import Domain.GroceryEntryId (GroceryEntryId)

type GroceryList = Array GroceryEntry

codec :: CA.JsonCodec GroceryList
codec =
  CA.array GroceryEntry.codec

upsertGrocery :: GroceryEntry -> GroceryList -> GroceryList
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
  hasId x = GroceryEntry.id x == GroceryEntry.id grocery
  update i = Array.updateAt i grocery groceryList
  consGroceryList _ = Array.cons grocery groceryList

toggleGrocery :: GroceryEntryId -> GroceryList -> GroceryList
toggleGrocery id groceryList =
  groceryList <#> toggle
  where
  toggle grocery
    | GroceryEntry.id grocery == id = GroceryEntry.toggleChecked grocery
    | otherwise = grocery

deleteGroceries :: Array GroceryEntry -> GroceryList -> GroceryList
deleteGroceries groceries groceryList =
  Array.difference groceryList groceries

updateGroceries :: (GroceryEntry -> GroceryEntry) -> GroceryList -> GroceryList
updateGroceries f groceryList =
  map f groceryList

partitionGroceriesOnChecked
  :: GroceryList -> { checked :: GroceryList, unchecked :: GroceryList }
partitionGroceriesOnChecked gs =
  { checked: partitioned.yes
  , unchecked: partitioned.no
  }
  where
  partitioned = Array.partition GroceryEntry.checked gs

set :: GroceryEntry -> GroceryList -> GroceryList
set grocery list = help <$> list
  where
  help g
    | GroceryEntry.id g == GroceryEntry.id grocery = grocery
    | otherwise = g

delete :: GroceryEntry -> GroceryList -> GroceryList
delete grocery list = Array.delete grocery list

insertAt :: Int -> GroceryEntry -> GroceryList -> GroceryList
insertAt index grocery list =
  Array.insertAt index grocery list
    # Maybe.fromMaybe list

clearCompleted :: GroceryList -> GroceryList
clearCompleted = Array.filter (not <<< GroceryEntry.checked)
