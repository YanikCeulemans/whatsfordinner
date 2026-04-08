module Domain.Grocery where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.GroceryId (GroceryId)
import Domain.GroceryId as GroceryId

type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

groceryCodec :: JsonCodec Grocery
groceryCodec =
  CA.object "Grocery"
    $ CAR.record
        { id: GroceryId.groceryIdCodec
        , description: CA.string
        , amount: Amount.amountCodec
        , checked: CA.boolean
        }

