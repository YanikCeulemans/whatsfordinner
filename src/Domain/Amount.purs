module Domain.Amount
  ( Amount(..)
  , append
  , codec
  , increaseWith
  , setValue
  , value
  , amountUnit
  , unitless
  , withUnit
  , toTaste
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Variant as V
import Type.Proxy (Proxy(..))

data Amount
  = WithUnit { value :: Number, unit :: String }
  | Unitless Number
  | ToTaste

derive instance Eq Amount

instance Show Amount where
  show = case _ of
    WithUnit x -> "WithUnit " <> show x
    Unitless x -> "Unitless " <> show x
    ToTaste -> "ToTaste"

codec :: JsonCodec Amount
codec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { withUnit:
        Right $ CA.object "withUnit" $
          CAR.record
            { value: dimap identity sanitize CA.number
            , unit: CA.string
            }
    , unitless: Right $ dimap identity sanitize CA.number
    , toTaste: Left unit
    }
  where
  toVariant = case _ of
    WithUnit x -> V.inj (Proxy :: _ "withUnit") x
    Unitless x -> V.inj (Proxy :: _ "unitless") x
    ToTaste -> V.inj (Proxy :: _ "toTaste") unit
  fromVariant = V.match
    { withUnit: WithUnit
    , unitless: Unitless
    , toTaste: \_ -> ToTaste
    }

sanitize :: Number -> Number
sanitize = max 1.0

unitless :: Number -> Amount
unitless = Unitless <<< sanitize

withUnit :: Number -> String -> Amount
withUnit x unit' =
  WithUnit
    { value: sanitize x
    , unit: unit'
    }

toTaste :: Amount
toTaste = ToTaste

append :: Amount -> Amount -> Maybe Amount
append a b = case a, b of
  WithUnit a', WithUnit b' | a'.unit == b'.unit -> Just $ WithUnit
    { unit: a'.unit, value: a'.value + b'.value }
  Unitless a', Unitless b' -> Just $ Unitless $ a' + b'
  ToTaste, ToTaste -> Just ToTaste
  _, _ -> Nothing

increaseWith :: Number -> Amount -> Amount
increaseWith delta = case _ of
  WithUnit x -> WithUnit $ x { value = sanitize $ x.value + delta }
  Unitless x -> Unitless $ sanitize $ x + delta
  ToTaste -> ToTaste

setValue :: Number -> Amount -> Amount
setValue amountVal = case _ of
  WithUnit x -> WithUnit $ x { value = sanitize amountVal }
  Unitless _ -> Unitless $ sanitize amountVal
  ToTaste -> ToTaste

value :: Amount -> Maybe Number
value = case _ of
  WithUnit x -> Just x.value
  Unitless x -> Just x
  ToTaste -> Nothing

amountUnit :: Amount -> Maybe String
amountUnit = case _ of
  WithUnit x -> Just x.unit
  Unitless _ -> Nothing
  ToTaste -> Nothing

