module Common.Id (Id(..), GenEnv, codec, print, parse, gen) where

import Prelude

import Common.Extensions.ULID as ULIDExt
import Data.Argonaut as J
import Data.Bifunctor (bimap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Effect (Effect)
import Simple.ULID (ULID)
import Simple.ULID as ULID
import Simple.ULID.Node as ULIDN
import Simple.ULID.Window as ULIDB

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
    bimap CA.TypeMismatch MkId $ ULIDExt.parse xStr

print :: forall a. Id a -> String
print (MkId id) = ULID.toString id

parse :: forall a. String -> Either String (Id a)
parse candidate =
  ULIDExt.parse candidate
    <#> MkId

data GenEnv
  = Browser
  | Node

gen :: forall a. GenEnv -> Effect (Id a)
gen genEnv =
  ULID.genULID prng <#> MkId
  where
  prng =
    case genEnv of
      Browser -> ULIDB.prng
      Node -> ULIDN.prng
