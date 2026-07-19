module Common.Ingredient where

import Common.Amount (Amount)
import Common.Amount as Amount
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CodecRecord

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
