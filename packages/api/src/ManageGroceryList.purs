module Api.ManageGroceryList where

import Prelude

import Common.GroceryList (GroceryEntry, GroceryList)
import Common.GroceryListId (GroceryListId)
import Data.Either (Either)
import Data.Maybe (Maybe)

data UpsertGroceryEntryError = NoSuchGroceryList GroceryListId

class Monad m <= ManageGroceryList m where
  loadGroceryList :: GroceryListId -> m (Maybe GroceryList)
  upsertGroceryList :: GroceryListId -> GroceryList -> m Unit
  upsertGroceryEntry
    :: GroceryListId -> GroceryEntry -> m (Either UpsertGroceryEntryError Unit)

