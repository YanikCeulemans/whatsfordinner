module Domain.Ingredient where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CodecRecord
import Domain.Amount (Amount)
import Domain.Amount as Amount

type Ingredient =
  { name :: String
  , amount :: Amount
  }

codec :: JsonCodec Ingredient
codec =
  CodecRecord.object "Ingredient"
    { name: Codec.string
    , amount: Amount.codec
    }
