module Domain.GroceryList where

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Domain.Grocery (Grocery)
import Domain.Grocery as Grocery

type GroceryList = Array Grocery

codec :: CA.JsonCodec GroceryList
codec =
  CA.array Grocery.codec

upsertGrocery :: Grocery -> GroceryList -> GroceryList
upsertGrocery grocery groceryList =
  {--  
    TODO: what is the proper identity of a grocery? 
    An arbitrary ULID id field doesn't seem to be the answer. What is the
    desired behaviour of adding multiple groceries to the grocery list?
      - Do we want the possiblity to have multiple lines with the same description
      - What happens when someone enters Carrots x5 when there is already Carrots 1kg on the list?
  --}
  Array.cons grocery groceryList

