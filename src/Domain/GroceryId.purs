module Domain.GroceryId where

import Prelude

import Data.Argonaut as J
import Data.Bifunctor (bimap, lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Profunctor (dimap)
import Data.ULID as DULID
import Simple.ULID (ULID)
import Simple.ULID as ULID

newtype GroceryId = MkGroceryId ULID

derive instance Eq GroceryId
derive newtype instance Show GroceryId

codec :: JsonCodec GroceryId
codec =
  CA.codec' wrap unwrap
  where
  unwrap :: GroceryId -> J.Json
  unwrap (MkGroceryId ulid) = J.fromString $ ULID.toString ulid

  wrap :: J.Json -> Either CA.JsonDecodeError GroceryId
  wrap x = do
    xStr <- Either.note (CA.TypeMismatch "string") $ J.toString x
    bimap CA.TypeMismatch MkGroceryId $ DULID.parse xStr

print :: GroceryId -> String
print (MkGroceryId id) = ULID.toString id
