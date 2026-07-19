module Common.MealScheduleId
  ( MealScheduleId(..)
  , MealSchedule
  , codec
  , parse
  , print
  ) where

import Prelude

import Common.Extensions.ULID as ULIDExt
import Common.Id (Id)
import Common.Id as Id
import Data.Codec.Argonaut (JsonCodec)
import Data.Either (Either)
import Data.Profunctor (dimap)

data MealSchedule

newtype MealScheduleId = MkMealScheduleId (Id MealSchedule)

derive instance Eq MealScheduleId

codec :: JsonCodec MealScheduleId
codec =
  dimap unwrap wrap Id.codec
  where
  unwrap (MkMealScheduleId id) = id
  wrap = MkMealScheduleId

parse :: String -> Either String MealScheduleId
parse = ULIDExt.parse >>> map Id.MkId >>> map MkMealScheduleId

print :: MealScheduleId -> String
print (MkMealScheduleId id) = Id.print id
