module Common.GroceryList
  ( GroceryEntry
  , GroceryList
  , upsertEntry
  , codec
  , entryCodec
  , deleteGroceries
  , entryDescription
  , entrySortIndex
  , updateEntries
  , createEmpty
  , toggleEntryChecked
  , set
  , entryChecked
  , clearCompleted
  , uncheckEntry
  , entryId
  , partitionGroceriesOnChecked
  , setEntrySortIndex
  , upsertGrocery
  , upsertGrocery'
  , entryAmount
  , updateGroceries'
  , groceries
  , isEmpty
  , lookup
  ) where

import Prelude

import Common.Amount (Amount)
import Common.Amount as Amount
import Common.GroceryEntryId (GroceryEntryId)
import Common.GroceryListId (GroceryListId)
import Common.Id as Id
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (maximum)
import Data.Foldable as Foldable
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Lens.Record as LensRecord
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Profunctor (dimap)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Type.Prelude (Proxy(..))

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

type GroceryListProduct =
  { id :: GroceryListId
  , data :: Map GroceryEntryId GroceryEntry
  }

newtype GroceryList = MkGroceryList GroceryListProduct

createEmpty :: GroceryListId -> GroceryList
createEmpty id = MkGroceryList { id, data: Map.empty }

_groceryList :: Lens' GroceryList GroceryListProduct
_groceryList = Lens.iso getter setter
  where
  getter (MkGroceryList x) = x
  setter = MkGroceryList

_data :: Lens' GroceryList (Map GroceryEntryId GroceryEntry)
_data = _groceryList <<< LensRecord.prop (Proxy @"data")

groceries :: GroceryList -> Array GroceryEntry
groceries = Lens.view _data >>> Map.values >>> Array.fromFoldable

isEmpty :: GroceryList -> Boolean
isEmpty = Lens.view _data >>> Map.isEmpty

lookup :: GroceryEntryId -> GroceryList -> Maybe GroceryEntry
lookup k = Lens.view _data >>> Map.lookup k

updateEntries :: (GroceryEntry -> GroceryEntry) -> GroceryList -> GroceryList
updateEntries f = Lens.over _data (map f)

codec :: CA.JsonCodec GroceryList
codec =
  dimap unwrap wrap
    $ CAR.object "GroceryList"
        { id: Id.codec
        , data: CAC.map Id.codec entryCodec
        }
  where
  unwrap (MkGroceryList x) = x
  wrap = MkGroceryList

-- | See `upsertEntry` but then given a sort index an entry id, description 
-- | and amount separately
upsertGrocery'
  :: Maybe Int
  -> GroceryEntryId
  -> String
  -> Amount
  -> GroceryList
  -> Tuple GroceryEntry GroceryList
upsertGrocery' sortIndex id description amount groceryList =
  entry /\ upserted
  where
  upserted = upsertEntry entry groceryList
  entry =
    MkGroceryEntry
      { id
      , description
      , amount
      , checked: false
      , sortIndex: Maybe.fromMaybe maxSortIndex $ sortIndex
      }
  maxSortIndex =
    Lens.view _data groceryList
      # map entrySortIndex
      # maximum
      # map (_ + 1)
      # Maybe.fromMaybe 0

-- | See `upsertEntry` but then given an entry id, description and amount
-- | separately
upsertGrocery
  :: GroceryEntryId
  -> String
  -> Amount
  -> GroceryList
  -> Tuple GroceryEntry GroceryList
upsertGrocery = upsertGrocery' Nothing

updateGroceries'
  :: (GroceryEntry -> Tuple Boolean GroceryEntry)
  -> GroceryList
  -> Tuple (Map GroceryEntryId GroceryEntry) GroceryList
updateGroceries' f groceryList = modifiedGroceries /\ modifiedGroceryList
  where
  groceries =
    Lens.view _data groceryList
      # map f
  keepModified (wasModified /\ x)
    | wasModified = Just x
    | otherwise = Nothing
  modifiedGroceries = Map.mapMaybe keepModified groceries
  allGroceries = snd <$> groceries
  modifiedGroceryList = Lens.set _data allGroceries groceryList

-- | Sets the given grocery entry in the grocery list, overwriting any existing
-- | grocery entry with the same entry id
set :: GroceryEntry -> GroceryList -> GroceryList
set groceryEntry@(MkGroceryEntry grocery) groceryList =
  Lens.over _data set' groceryList
  where
  set' = Map.insert grocery.id groceryEntry

-- | Inserts the given `GroceryEntry` unless it already exists by id or 
-- | description. If it already exists, the amount is updated with the given
-- | entry's amount if possible.
upsertEntry :: GroceryEntry -> GroceryList -> GroceryList
upsertEntry groceryEntry groceryList =
  Lens.over _data upsertEntry' groceryList
  where
  upsertEntry' groceries =
    Map.alter (alter groceries) (entryId groceryEntry) groceries
  alter groceries existingEntry =
    Foldable.oneOf
      [ map updateAmount existingEntry
      , Map.values groceries
          # List.find descriptionMatches
          # map updateAmount
      , Just groceryEntry
      ]
  updateAmount ge@(MkGroceryEntry g) =
    case Amount.append (entryAmount ge) (entryAmount groceryEntry) of
      Just newAmount -> MkGroceryEntry $ g { amount = newAmount }
      Nothing -> ge
  descriptionMatches g = (CaseInsensitiveString $ entryDescription g) ==
    (CaseInsensitiveString $ entryDescription groceryEntry)

deleteGroceries :: Array GroceryEntry -> GroceryList -> GroceryList
deleteGroceries groceriesToDelete groceryList =
  Lens.over _data deleteGroceriesHelp groceryList
  where
  deleteGroceriesHelp groceries = Map.difference groceries groceriesToDeleteMap
  groceriesToDeleteMap =
    groceriesToDelete
      # map group
      # Map.fromFoldable
  group entry = Tuple (entryId entry) entry

partitionGroceriesOnChecked
  :: GroceryList
  -> { checked :: Array GroceryEntry, unchecked :: Array GroceryEntry }
partitionGroceriesOnChecked groceryList =
  { checked: partitioned.yes
  , unchecked: partitioned.no
  }
  where
  partitioned =
    Lens.view _data groceryList
      # Array.fromFoldable
      # Array.partition entryChecked

clearCompleted :: GroceryList -> GroceryList
clearCompleted = Lens.over _data $ Map.filter (not <<< entryChecked)
