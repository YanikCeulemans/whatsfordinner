module Domain.PlannedMeal where

import Prelude

import Data.Foldable (fold)
import Domain.Ingredient (Ingredient)
import Domain.Meal (Meal)
import Domain.Meal as Meal

data PlannedMeal
  = NoMealPlanned
  | PlannedMeal Meal

derive instance Eq PlannedMeal
instance Show PlannedMeal where
  show = case _ of
    NoMealPlanned -> "NoMealPlanned"
    PlannedMeal a -> fold [ "PlannedMeal: ", show a ]

ingredients :: PlannedMeal -> Array Ingredient
ingredients = case _ of
  NoMealPlanned -> []
  PlannedMeal meal -> Meal.ingredients meal

