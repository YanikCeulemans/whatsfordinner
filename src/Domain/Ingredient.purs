module Domain.Ingredient where

import Domain.Amount (Amount)

type Ingredient =
  { name :: String
  , amount :: Amount
  }
