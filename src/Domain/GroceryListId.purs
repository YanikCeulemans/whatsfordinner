module Domain.GroceryListId where

import Domain.Id (Id)

data GroceryList

type GroceryListId = Id GroceryList
