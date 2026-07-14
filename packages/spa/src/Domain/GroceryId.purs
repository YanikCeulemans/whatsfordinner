module Spa.Domain.GroceryId where

import Spa.Domain.Id (Id)

data Grocery

type GroceryId = Id Grocery
