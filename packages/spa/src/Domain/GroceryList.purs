module Spa.Domain.GroceryList where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (foldr, maximum)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Profunctor (dimap)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Spa.Domain.Amount (Amount)
import Spa.Domain.Amount as Amount
import Spa.Domain.GroceryEntryId (GroceryEntryId)
import Spa.Domain.Id as Id

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
toggleEntryChecked (MkGroceryEntry grocery) =
  MkGroceryEntry $ grocery { checked = not grocery.checked }

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

upsertGrocery'
  :: Maybe Int
  -> GroceryEntryId
  -> String
  -> Amount
  -> GroceryList
  -> Tuple GroceryEntry GroceryList
upsertGrocery' sortIndex id description amount groceryList =
  (Array.findIndex hasId groceryList >>= update)
    # Maybe.fromMaybe' consGroceryList
    # Tuple newGrocery
  where
  hasId x = entryId x == id
  upsert (MkGroceryEntry grocery) =
    pure $ MkGroceryEntry $ grocery
      { description = description
      , amount = amount
      , sortIndex = sortIndex # Maybe.fromMaybe grocery.sortIndex
      }
  update i = Array.alterAt i upsert groceryList
  newEntrySortIndex _ =
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
      , sortIndex: sortIndex # Maybe.fromMaybe' newEntrySortIndex
      }
  consGroceryList _ = Array.cons newGrocery groceryList

upsertGrocery
  :: GroceryEntryId
  -> String
  -> Amount
  -> GroceryList
  -> Tuple GroceryEntry GroceryList
upsertGrocery = upsertGrocery' Nothing

updateBy :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
updateBy pred updater xs =
  Array.findIndex pred xs >>= update
  where
  help = updater >>> Just
  update i = Array.alterAt i help xs

upsertEntry :: GroceryEntry -> GroceryList -> GroceryList
upsertEntry entry groceryList =
  updateBy (eq entry) update groceryList
    <|> updateBy descriptionMatches updateAmount groceryList
    # Maybe.fromMaybe' insert
  where
  updateAmount ge@(MkGroceryEntry g) =
    case Amount.append (entryAmount ge) (entryAmount entry) of
      Just newAmount -> MkGroceryEntry $ g { amount = newAmount }
      Nothing -> ge
  descriptionMatches g = (CaseInsensitiveString $ entryDescription g) ==
    (CaseInsensitiveString $ entryDescription entry)
  update _ = entry
  insert _ = Array.cons entry groceryList

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

updateGroceries'
  :: (GroceryEntry -> Tuple Boolean GroceryEntry)
  -> GroceryList
  -> Tuple (Array GroceryEntry) GroceryList
updateGroceries' f =
  foldr go ([] /\ [])
  where
  go curr (modified /\ groceryList) =
    case f curr of
      true /\ updated -> (updated : modified) /\ updated : groceryList
      false /\ _ -> modified /\ curr : groceryList

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
