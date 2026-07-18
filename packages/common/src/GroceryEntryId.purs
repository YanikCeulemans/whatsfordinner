module Common.GroceryEntryId where

import Common.Id (Id)

data GroceryEntry

type GroceryEntryId = Id GroceryEntry
