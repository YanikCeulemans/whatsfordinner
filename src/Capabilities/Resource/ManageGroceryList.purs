module Capabilities.Resource.ManageGroceryList where

import Prelude

import Domain.Grocery (Grocery)
import Domain.GroceryListId (GroceryListId)
import Halogen as H

class Monad m <= ManageGroceryList m where
  upsertGrocery :: GroceryListId -> Grocery -> m Unit

instance
  ManageGroceryList m =>
  ManageGroceryList (H.HalogenM st act slots msg m) where
  upsertGrocery id = H.lift <<< upsertGrocery id
