module Spa.Domain.MealSchedule where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CodecRecord
import Data.Date (Date)
import Data.Date as Date
import Data.Int as Int
import Data.List (List)
import Data.Profunctor (dimap)
import Data.Time.Duration (Days(..))
import Spa.Domain.Extensions.Date as DateExtensions
import Spa.Domain.MealScheduleId (MealScheduleId)
import Spa.Domain.MealScheduleId as MealSchedule
import Spa.Domain.PlannedMeal (PlannedMeal)
import Spa.Domain.PlannedMeal as PlannedMeal
import Spa.Domain.Range (Range)
import Spa.Domain.Range as Range
import Spa.Domain.RingList (RingList)
import Spa.Domain.RingList as RingList

newtype MealSchedule = MkMealSchedule
  { id :: MealScheduleId, startDate :: Date, schedule :: RingList PlannedMeal }

codec :: JsonCodec MealSchedule
codec =
  dimap unwrap wrap $ CodecRecord.object "MealSchedule"
    { id: MealSchedule.codec
    , startDate: DateExtensions.codec
    , schedule: RingList.codec PlannedMeal.codec
    }
  where
  unwrap (MkMealSchedule mealSchedule) = mealSchedule
  wrap = MkMealSchedule

toList :: Range Date -> MealSchedule -> List PlannedMeal
toList dateRange (MkMealSchedule { startDate, schedule }) =
  RingList.toList range schedule
  where
  rangeStart = Range.start dateRange
  rangeEnd = Range.end dateRange
  Days rangeDiff = Date.diff rangeEnd rangeStart
  Days indexStart = Date.diff rangeStart startDate
  range = Range.create (Int.round indexStart) $
    (Int.round $ indexStart + rangeDiff)

asList :: MealSchedule -> List PlannedMeal
asList (MkMealSchedule { schedule }) = RingList.asList schedule
