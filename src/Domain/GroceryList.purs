module Domain.GroceryList where

import Data.Codec.Argonaut as CA
import Domain.Grocery (Grocery)
import Domain.Grocery as Grocery

type GroceryList = Array Grocery

codec :: CA.JsonCodec GroceryList
codec =
  CA.array Grocery.codec
