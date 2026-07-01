module Domain.MealScheduleId (MealScheduleId(..), MealSchedule, codec) where

import Data.Codec.Argonaut (JsonCodec)
import Data.Profunctor (dimap)
import Domain.Id (Id)
import Domain.Id as Id

data MealSchedule

newtype MealScheduleId = MkMealScheduleId (Id MealSchedule)

codec :: JsonCodec MealScheduleId
codec =
  dimap unwrap wrap Id.codec
  where
  unwrap (MkMealScheduleId id) = id
  wrap = MkMealScheduleId
