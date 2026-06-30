module Domain.MealSchedule where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Common as CodecCommon
import Data.Codec.Argonaut.Record as CodecRecord
import Data.Date (Date)
import Data.Date as Date
import Data.Int as Int
import Data.List (List)
import Data.Time.Duration (Days(..))
import Domain.Extensions.Date as DateExtensions
import Domain.Id as Id
import Domain.MealScheduleId (MealScheduleId)
import Domain.PlannedMeal (PlannedMeal)
import Domain.Range (Range)
import Domain.Range as Range
import Domain.RingList (RingList)
import Domain.RingList as RingList

newtype MealSchedule = MkMealSchedule
  { id :: MealScheduleId, startDate :: Date, schedule :: RingList PlannedMeal }

codec :: JsonCodec MealSchedule
codec =
  CodecRecord.object
    { id: Id.codec
    , startDate: DateExtensions.codec
    , schedule: RingList.codec PlannedMeal.codec
    }

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

