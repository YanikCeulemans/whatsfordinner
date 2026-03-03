module Domain.PlannedMeal where

import Prelude

import Data.Date (Date)
import Data.Foldable (fold)
import Domain.Meal (Meal)
import Domain.RingList (RingList)

data PlannedMeal
  = NoMealPlanned
  | PlannedMeal Meal

derive instance Eq PlannedMeal
instance Show PlannedMeal where
  show = case _ of
    NoMealPlanned -> "NoMealPlanned"
    PlannedMeal a -> fold [ "PlannedMeal: ", show a]

