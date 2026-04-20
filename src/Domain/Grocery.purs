module Domain.Grocery
  ( Grocery
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
import Domain.GroceryId (GroceryId)
import Domain.GroceryId as GroceryId

newtype Grocery = MkGrocery
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

derive newtype instance Show Grocery
instance Eq Grocery where
  eq (MkGrocery a) (MkGrocery b) = a.id == b.id

create :: GroceryId -> String -> Amount -> Grocery
create id' description' amount' =
  MkGrocery
    { id: id'
    , description: description'
    , amount: amount'
    , checked: false
    }

codec :: JsonCodec Grocery
codec =
  dimap unwrap wrap
    $ CA.object "Grocery"
    $ CAR.record
        { id: GroceryId.codec
        , description: CA.string
        , amount: Amount.codec
        , checked: CA.boolean
        }
  where
  unwrap (MkGrocery grocery) = grocery
  wrap = MkGrocery

id :: Grocery -> GroceryId
id (MkGrocery grocery) = grocery.id

checked :: Grocery -> Boolean
checked (MkGrocery grocery) = grocery.checked

description :: Grocery -> String
description (MkGrocery grocery) = grocery.description

amount :: Grocery -> Amount
amount (MkGrocery grocery) = grocery.amount

toggleChecked :: Grocery -> Grocery
toggleChecked (MkGrocery grocery) = MkGrocery $ grocery
  { checked = not grocery.checked }

uncheck :: Grocery -> Grocery
uncheck (MkGrocery grocery) = MkGrocery $ grocery { checked = false }
