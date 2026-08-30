module Api.Publisher where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)

class Monad m <= Publisher m where
  publish :: forall a. EncodeJson a => a -> m Unit
  publishJson :: Json -> m Unit

defaultPublish :: forall m a. Publisher m => EncodeJson a => a -> m Unit
defaultPublish = encodeJson >>> publishJson
