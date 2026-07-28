module Api.JsonBody (JsonBody, create) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array.NonEmpty (fold1)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Codec.Argonaut (Codec)
import Data.Codec.Argonaut as JsonCodec
import Data.MediaType.Common (applicationJSON)
import HTTPurple.Body (class Body, defaultHeaders, write)
import HTTPurple.Headers as RequestHeaders

newtype JsonBody = MkJsonBody String

instance Body JsonBody where
  write (MkJsonBody json) = write json

  defaultHeaders (MkJsonBody json) = do
    stringHeaders <- defaultHeaders json

    pure $ fold1 $ NonEmptyArray
      [ stringHeaders
      , RequestHeaders.mkRequestHeader "Content-Type" $ show
          applicationJSON
      ]

create :: forall m a c d. Codec m a Json c d → c → JsonBody
create codec = JsonCodec.encode codec >>> Json.stringify >>> MkJsonBody
