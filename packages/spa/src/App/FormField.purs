module Spa.App.FormField where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML.Properties as HP

data FormField
  = Pristine
  | Invalid String
  | Valid String

fieldValue :: FormField -> String
fieldValue = case _ of
  Pristine -> ""
  Invalid v -> v
  Valid v -> v

isFieldValid :: FormField -> Boolean
isFieldValid = case _ of
  Valid _ -> true
  _ -> false

validFieldValue :: FormField -> Maybe String
validFieldValue = case _ of
  Valid v -> Just v
  _ -> Nothing

ariaValid :: forall r i. FormField -> Array (HP.IProp r i)
ariaValid = case _ of
  Pristine -> []
  Invalid _ -> [ HP.attr (H.AttrName "aria-valid") "false" ]
  Valid _ -> [ HP.attr (H.AttrName "aria-valid") "true" ]

ariaInvalid :: forall r i. FormField -> Array (HP.IProp r i)
ariaInvalid = case _ of
  Pristine -> []
  Invalid _ -> [ HP.attr (H.AttrName "aria-invalid") "true" ]
  Valid _ -> [ HP.attr (H.AttrName "aria-invalid") "false" ]

ariaValidity :: forall r i. FormField -> Array (HP.IProp r i)
ariaValidity formField = append valid invalid
  where
  valid = ariaValid formField
  invalid = ariaInvalid formField
