module Domain.GroceryEntry
  ( GroceryEntry
  , create
  , codec
  , id
  , checked
  , description
  , amount
  , toggleChecked
  , uncheck
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (dimap)
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.GroceryEntryId (GroceryEntryId)
import Domain.Id as Id

newtype GroceryEntry = MkGroceryEntry
  { id :: GroceryEntryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

derive newtype instance Show GroceryEntry
instance Eq GroceryEntry where
  eq (MkGroceryEntry a) (MkGroceryEntry b) = a.id == b.id

create :: GroceryEntryId -> String -> Amount -> GroceryEntry
create id' description' amount' =
  MkGroceryEntry
    { id: id'
    , description: description'
    , amount: amount'
    , checked: false
    }

codec :: JsonCodec GroceryEntry
codec =
  dimap unwrap wrap
    $ CA.object "Grocery"
    $ CAR.record
        { id: Id.codec
        , description: CA.string
        , amount: Amount.codec
        , checked: CA.boolean
        }
  where
  unwrap (MkGroceryEntry grocery) = grocery
  wrap = MkGroceryEntry

id :: GroceryEntry -> GroceryEntryId
id (MkGroceryEntry grocery) = grocery.id

checked :: GroceryEntry -> Boolean
checked (MkGroceryEntry grocery) = grocery.checked

description :: GroceryEntry -> String
description (MkGroceryEntry grocery) = grocery.description

amount :: GroceryEntry -> Amount
amount (MkGroceryEntry grocery) = grocery.amount

toggleChecked :: GroceryEntry -> GroceryEntry
toggleChecked (MkGroceryEntry grocery) = MkGroceryEntry $ grocery
  { checked = not grocery.checked }

uncheck :: GroceryEntry -> GroceryEntry
uncheck (MkGroceryEntry grocery) = MkGroceryEntry $ grocery { checked = false }
