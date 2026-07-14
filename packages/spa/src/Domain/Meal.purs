module Spa.Domain.Meal (Meal(..), ingredients, codec) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CodecRecord
import Data.Profunctor (dimap)
import Spa.Domain.Ingredient (Ingredient)
import Spa.Domain.Ingredient as Ingredient

newtype Meal =
  MkMeal
    { name :: String
    , ingredients :: Array Ingredient
    }

derive instance Eq Meal

instance Show Meal where
  show (MkMeal { name }) = name

codec :: JsonCodec Meal
codec =
  dimap unwrap wrap $ CodecRecord.object "Meal"
    { name: Codec.string
    , ingredients: Codec.array Ingredient.codec
    }
  where
  unwrap (MkMeal meal) = meal
  wrap = MkMeal

ingredients :: Meal -> Array Ingredient
ingredients (MkMeal meal) = meal.ingredients
