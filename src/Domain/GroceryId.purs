module Domain.GroceryId where

import Domain.Id (Id)

data Grocery

type GroceryId = Id Grocery
