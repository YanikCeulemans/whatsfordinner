module Common.PlannedMeal where

import Prelude

import Common.Ingredient (Ingredient)
import Common.Meal (Meal)
import Common.Meal as Meal
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Variant as CodecVariant
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Profunctor (dimap)
import Data.Variant as V
import Type.Prelude (Proxy(..))

data PlannedMeal
  = NoMealPlanned
  | PlannedMeal Meal

derive instance Eq PlannedMeal
instance Show PlannedMeal where
  show = case _ of
    NoMealPlanned -> "NoMealPlanned"
    PlannedMeal a -> fold [ "PlannedMeal: ", show a ]

codec :: JsonCodec PlannedMeal
codec =
  dimap toVariant fromVariant $ CodecVariant.variantMatch
    { noMealPlanned: Left unit
    , plannedMeal: Right Meal.codec
    }
  where
  toVariant = case _ of
    NoMealPlanned -> V.inj (Proxy :: _ "noMealPlanned") unit
    PlannedMeal meal -> V.inj (Proxy :: _ "plannedMeal") meal
  fromVariant = V.match
    { noMealPlanned: \_ -> NoMealPlanned
    , plannedMeal: PlannedMeal
    }

ingredients :: PlannedMeal -> Array Ingredient
ingredients = case _ of
  NoMealPlanned -> []
  PlannedMeal meal -> Meal.ingredients meal
