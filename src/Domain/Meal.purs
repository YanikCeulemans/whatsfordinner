module Domain.Meal where

import Prelude

import Data.Foldable (intercalate)

newtype Meal = MkMeal { name :: String }
derive instance Eq Meal

instance Show Meal where
  show (MkMeal { name }) = intercalate ": " ["name", name]

