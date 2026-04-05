module Domain.Grocery where

import Domain.Amount (Amount)
import Domain.GroceryId (GroceryId)

type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

