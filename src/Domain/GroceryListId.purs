module Domain.GroceryListId where

import Prelude

import Data.Argonaut as J
import Data.Bifunctor (bimap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Data.ULID as DULID
import Simple.ULID (ULID)
import Simple.ULID as ULID

newtype GroceryListId = MkGroceryListId ULID

derive instance Eq GroceryListId
derive newtype instance Show GroceryListId
derive newtype instance Ord GroceryListId

codec :: JsonCodec GroceryListId
codec =
  CA.codec' wrap unwrap
  where
  unwrap :: GroceryListId -> J.Json
  unwrap (MkGroceryListId ulid) = J.fromString $ ULID.toString ulid

  wrap :: J.Json -> Either CA.JsonDecodeError GroceryListId
  wrap x = do
    xStr <- Either.note (CA.TypeMismatch "string") $ J.toString x
    bimap CA.TypeMismatch MkGroceryListId $ DULID.parse xStr

print :: GroceryListId -> String
print (MkGroceryListId id) = ULID.toString id
