module Spa.Domain.GroceryEntryId where

import Spa.Domain.Id (Id)

data GroceryEntry

type GroceryEntryId = Id GroceryEntry
