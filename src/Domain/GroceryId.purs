module Domain.GroceryId where

import Prelude

import Simple.ULID (ULID)
import Simple.ULID as ULID

newtype GroceryId = MkGroceryId ULID

derive instance Eq GroceryId
derive newtype instance Show GroceryId

print :: GroceryId -> String
print (MkGroceryId id) = ULID.toString id
