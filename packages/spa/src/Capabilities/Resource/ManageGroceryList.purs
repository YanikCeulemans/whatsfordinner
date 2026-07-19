module Spa.Capabilities.Resource.ManageGroceryList where

import Prelude

import Common.GroceryList (GroceryEntry, GroceryList)
import Common.GroceryListId (GroceryListId)
import Halogen as H

type SortedGrocery =
  { description :: String
  , sortIndex :: Int
  }

{-- TODO: 
    These signatures could be improved, it could be -> m (Either RemoteError GroceryList)
--}
class Monad m <= ManageGroceryList m where
  upsertGroceryList :: GroceryListId -> m GroceryList
  upsertGrocery :: GroceryListId -> GroceryEntry -> m Unit
  upsertGroceries :: GroceryListId -> Array GroceryEntry -> m GroceryList
  deleteGroceries :: GroceryListId -> Array GroceryEntry -> m GroceryList
  updateGroceries
    :: GroceryListId -> (GroceryEntry -> GroceryEntry) -> m GroceryList
  suggestGroceries :: String -> m (Array SortedGrocery)

instance
  ManageGroceryList m =>
  ManageGroceryList (H.HalogenM st act slots msg m) where
  upsertGroceryList = H.lift <<< upsertGroceryList
  upsertGrocery id = H.lift <<< upsertGrocery id
  upsertGroceries id = H.lift <<< upsertGroceries id
  deleteGroceries id = H.lift <<< deleteGroceries id
  updateGroceries id = H.lift <<< updateGroceries id
  suggestGroceries = H.lift <<< suggestGroceries
