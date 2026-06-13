module Domain.GroceryList where

import Prelude

import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (maximum)
import Data.Maybe as Maybe
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..))
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.GroceryEntryId (GroceryEntryId)
import Domain.Id as Id

newtype GroceryEntry = MkGroceryEntry
  { id :: GroceryEntryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  , sortIndex :: Int
  }

derive newtype instance Show GroceryEntry
instance Eq GroceryEntry where
  eq (MkGroceryEntry a) (MkGroceryEntry b) = a.id == b.id

entryCodec :: JsonCodec GroceryEntry
entryCodec =
  dimap unwrap wrap
    $ CA.object "Grocery"
    $ CAR.record
        { id: Id.codec
        , description: CA.string
        , amount: Amount.codec
        , checked: CA.boolean
        , sortIndex: CA.int
        }
  where
  unwrap (MkGroceryEntry grocery) = grocery
  wrap = MkGroceryEntry

entryId :: GroceryEntry -> GroceryEntryId
entryId (MkGroceryEntry grocery) = grocery.id

entryChecked :: GroceryEntry -> Boolean
entryChecked (MkGroceryEntry grocery) = grocery.checked

entryDescription :: GroceryEntry -> String
entryDescription (MkGroceryEntry grocery) = grocery.description

entryAmount :: GroceryEntry -> Amount
entryAmount (MkGroceryEntry grocery) = grocery.amount

toggleEntryChecked :: GroceryEntry -> GroceryEntry
toggleEntryChecked (MkGroceryEntry grocery) = MkGroceryEntry $ grocery
  { checked = not grocery.checked }

uncheckEntry :: GroceryEntry -> GroceryEntry
uncheckEntry (MkGroceryEntry grocery) = MkGroceryEntry $ grocery
  { checked = false }

entrySortIndex :: GroceryEntry -> Int
entrySortIndex (MkGroceryEntry grocery) = grocery.sortIndex

setEntrySortIndex :: Int -> GroceryEntry -> GroceryEntry
setEntrySortIndex sortIndex (MkGroceryEntry entry) =
  MkGroceryEntry $ entry { sortIndex = sortIndex }

type GroceryList = Array GroceryEntry

codec :: CA.JsonCodec GroceryList
codec = CA.array entryCodec

upsertGrocery
  :: GroceryEntryId
  -> String
  -> Amount
  -> GroceryList
  -> Tuple GroceryEntry GroceryList
upsertGrocery id description amount groceryList =
  {--  
    TODO: what is the proper identity of a grocery? 
    An arbitrary ULID id field doesn't seem to be the answer. What is the
    desired behaviour of adding multiple groceries to the grocery list?
      - Do we want the possiblity to have multiple lines with the same description
      - What happens when someone enters Carrots x5 when there is already Carrots 1kg on the list?
  --}
  (Array.findIndex hasId groceryList >>= update)
    # Maybe.fromMaybe' consGroceryList
    # Tuple newGrocery
  where
  hasId x = entryId x == id
  upsert (MkGroceryEntry grocery) =
    pure $ MkGroceryEntry $ grocery
      { description = description
      , amount = amount
      }
  update i = Array.alterAt i upsert groceryList
  newEntrySortIndex =
    groceryList
      # map entrySortIndex
      # maximum
      # map (_ + 1)
      # Maybe.fromMaybe 0
  newGrocery =
    MkGroceryEntry
      { id
      , description
      , amount
      , checked: false
      , sortIndex: newEntrySortIndex
      }
  consGroceryList _ = Array.cons newGrocery groceryList

toggleGrocery :: GroceryEntryId -> GroceryList -> GroceryList
toggleGrocery id groceryList =
  groceryList <#> toggle
  where
  toggle grocery
    | entryId grocery == id = toggleEntryChecked grocery
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
  partitioned = Array.partition entryChecked gs

set :: GroceryEntry -> GroceryList -> GroceryList
set grocery list = help <$> list
  where
  help g
    | g == grocery = grocery
    | otherwise = g

delete :: GroceryEntry -> GroceryList -> GroceryList
delete grocery list = Array.delete grocery list

insertAt :: Int -> GroceryEntry -> GroceryList -> GroceryList
insertAt index grocery list =
  Array.insertAt index grocery list
    # Maybe.fromMaybe list

clearCompleted :: GroceryList -> GroceryList
clearCompleted = Array.filter (not <<< entryChecked)
