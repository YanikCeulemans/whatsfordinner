module Domain.Grocery where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.GroceryId (GroceryId)
import Domain.GroceryId as GroceryId

-- TODO: newtype with custom eq instance by id
type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

codec :: JsonCodec Grocery
codec =
  CA.object "Grocery"
    $ CAR.record
        { id: GroceryId.codec
        , description: CA.string
        , amount: Amount.codec
        , checked: CA.boolean
        }

id :: Grocery -> GroceryId
id grocery = grocery.id

checked :: Grocery -> Boolean
checked g = g.checked

toggleChecked :: Grocery -> Grocery
toggleChecked grocery = grocery { checked = not grocery.checked }
