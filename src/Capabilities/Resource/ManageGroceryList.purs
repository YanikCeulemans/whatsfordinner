module Capabilities.Resource.ManageGroceryList where

import Prelude

import Domain.GroceryEntry (GroceryEntry)
import Domain.GroceryList (GroceryList)
import Domain.GroceryListId (GroceryListId)
import Halogen as H

{-- TODO: 
    These signatures could be improved, it could be -> m (Maybe GroceryList)
--}
class Monad m <= ManageGroceryList m where
  upsertGroceryList :: GroceryListId -> m GroceryList
  upsertGrocery :: GroceryListId -> GroceryEntry -> m Unit
  deleteGroceries :: GroceryListId -> Array GroceryEntry -> m GroceryList
  updateGroceries
    :: GroceryListId -> (GroceryEntry -> GroceryEntry) -> m GroceryList

instance
  ManageGroceryList m =>
  ManageGroceryList (H.HalogenM st act slots msg m) where
  upsertGroceryList = H.lift <<< upsertGroceryList
  upsertGrocery id = H.lift <<< upsertGrocery id
  deleteGroceries id = H.lift <<< deleteGroceries id
  updateGroceries id = H.lift <<< updateGroceries id
