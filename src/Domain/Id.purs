module Domain.Id (Id(..), codec, print) where

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

newtype Id :: Type -> Type
newtype Id a = MkId ULID

derive instance Eq (Id a)
derive newtype instance Show (Id a)
derive newtype instance Ord (Id a)

codec :: forall a. JsonCodec (Id a)
codec =
  CA.codec' wrap unwrap
  where
  unwrap :: Id a -> J.Json
  unwrap (MkId ulid) = J.fromString $ ULID.toString ulid

  wrap :: J.Json -> Either CA.JsonDecodeError (Id a)
  wrap x = do
    xStr <- Either.note (CA.TypeMismatch "string") $ J.toString x
    bimap CA.TypeMismatch MkId $ DULID.parse xStr

print :: forall a. Id a -> String
print (MkId id) = ULID.toString id
