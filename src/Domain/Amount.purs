module Domain.Amount
  ( Amount
  , increaseWith
  , setValue
  , value
  , unit
  , unitless
  , create
  ) where

import Prelude

import Data.Maybe (Maybe(..))

newtype Amount = MkAmount
  { value :: Number
  , unit :: Maybe String
  }

derive newtype instance Show Amount

sanitize :: Number -> Number
sanitize = max 1.0

unitless :: Number -> Amount
unitless x = MkAmount
  { value: sanitize x
  , unit: Nothing
  }

create :: Number -> String -> Amount
create x unit' = MkAmount
  { value: sanitize x
  , unit: Just unit'
  }

increaseWith :: Number -> Amount -> Amount
increaseWith delta (MkAmount amount) = MkAmount $ amount
  { value = max 1.0 $ amount.value + delta }

setValue :: Number -> Amount -> Amount
setValue amountVal (MkAmount amount) = MkAmount $ amount
  { value = max 1.0 amountVal }

value :: Amount -> Number
value (MkAmount x) = x.value

unit :: Amount -> Maybe String
unit (MkAmount x) = x.unit

