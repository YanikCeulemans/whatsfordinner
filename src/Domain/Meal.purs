module Domain.Meal (Meal(..), ingredients) where

import Prelude

import Domain.Ingredient (Ingredient)

newtype Meal =
  MkMeal
    { name :: String
    , ingredients :: Array Ingredient
    }

derive instance Eq Meal

instance Show Meal where
  show (MkMeal { name }) = name

ingredients :: Meal -> Array Ingredient
ingredients (MkMeal meal) = meal.ingredients

