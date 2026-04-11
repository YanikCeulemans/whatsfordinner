module Capabilities.Resource.ManageGroceryList where

import Prelude

import Domain.Grocery (Grocery)
import Domain.GroceryList (GroceryList)
import Domain.GroceryListId (GroceryListId)
import Halogen as H

class Monad m <= ManageGroceryList m where
  upsertGroceryList :: GroceryListId -> m GroceryList
  upsertGrocery :: GroceryListId -> Grocery -> m Unit

instance
  ManageGroceryList m =>
  ManageGroceryList (H.HalogenM st act slots msg m) where
  upsertGroceryList = H.lift <<< upsertGroceryList
  upsertGrocery id = H.lift <<< upsertGrocery id
