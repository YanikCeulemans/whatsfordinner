module Domain.GroceryEntryId where

import Domain.Id (Id)

data GroceryEntry

type GroceryEntryId = Id GroceryEntry
