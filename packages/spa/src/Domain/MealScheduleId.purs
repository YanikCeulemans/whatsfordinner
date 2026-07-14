module Spa.Domain.MealScheduleId
  ( MealScheduleId(..)
  , MealSchedule
  , codec
  , parse
  , print
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Spa.Data.ULID as DULID
import Spa.Domain.Id (Id)
import Spa.Domain.Id as Id

data MealSchedule

newtype MealScheduleId = MkMealScheduleId (Id MealSchedule)

derive instance Eq MealScheduleId

codec :: JsonCodec MealScheduleId
codec =
  dimap unwrap wrap Id.codec
  where
  unwrap (MkMealScheduleId id) = id
  wrap = MkMealScheduleId

parse :: String -> Maybe MealScheduleId
parse = DULID.parse >>> map Id.MkId >>> map MkMealScheduleId >>> Either.hush

print :: MealScheduleId -> String
print (MkMealScheduleId id) = Id.print id
