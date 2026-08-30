module Api.Publisher where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec

class Monad m <= Publisher id m where
  publishJson :: id -> Json -> m Unit

publishEncode
  :: forall m a id. Publisher id m => EncodeJson a => id -> a -> m Unit
publishEncode id = encodeJson >>> publishJson id

publishCodec
  :: forall m a id. Publisher id m => id -> JsonCodec a -> a -> m Unit
publishCodec id codec = Codec.encode codec >>> publishJson id
