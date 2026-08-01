module Api.ManageGroceryList where

import Prelude

import Common.GroceryList (GroceryList)
import Common.GroceryListId (GroceryListId)
import Data.Maybe (Maybe)

class Monad m <= ManageGroceryList m where
  loadGroceryList :: GroceryListId -> m (Maybe GroceryList)

