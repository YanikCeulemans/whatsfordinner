module Capabilities.Resource.ManageGroceryList where

import Prelude

import Domain.Grocery (Grocery)
import Domain.GroceryList (GroceryList)
import Domain.GroceryListId (GroceryListId)
import Halogen as H

-- TODO: These signatures are lies, it should at least be -> m (Maybe GroceryList)
class Monad m <= ManageGroceryList m where
  upsertGroceryList :: GroceryListId -> m GroceryList
  upsertGrocery :: GroceryListId -> Grocery -> m Unit
  deleteGroceries :: GroceryListId -> Array Grocery -> m GroceryList
  updateGroceries :: GroceryListId -> (Grocery -> Grocery) -> m GroceryList

instance
  ManageGroceryList m =>
  ManageGroceryList (H.HalogenM st act slots msg m) where
  upsertGroceryList = H.lift <<< upsertGroceryList
  upsertGrocery id = H.lift <<< upsertGrocery id
  deleteGroceries id = H.lift <<< deleteGroceries id
  updateGroceries id = H.lift <<< updateGroceries id
