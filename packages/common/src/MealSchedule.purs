module Common.MealSchedule where

import Prelude

import Common.Extensions.Date as DateExt
import Common.MealScheduleId (MealScheduleId)
import Common.MealScheduleId as MealSchedule
import Common.PlannedMeal (PlannedMeal)
import Common.PlannedMeal as PlannedMeal
import Common.Range (Range)
import Common.Range as Range
import Common.RingList (RingList)
import Common.RingList as RingList
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CodecRecord
import Data.Date (Date)
import Data.Date as Date
import Data.Int as Int
import Data.List (List)
import Data.Profunctor (dimap)
import Data.Time.Duration (Days(..))

newtype MealSchedule = MkMealSchedule
  { id :: MealScheduleId, startDate :: Date, schedule :: RingList PlannedMeal }

codec :: JsonCodec MealSchedule
codec =
  dimap unwrap wrap $ CodecRecord.object "MealSchedule"
    { id: MealSchedule.codec
    , startDate: DateExt.codec
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
