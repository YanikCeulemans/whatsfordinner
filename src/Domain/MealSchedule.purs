module Domain.MealSchedule where

import Prelude

import Data.Date (Date)
import Data.Date as Date
import Data.Int as Int
import Data.List (List(..))
import Data.Time.Duration (Days(..))
import Domain.PlannedMeal (PlannedMeal)
import Domain.Range (Range)
import Domain.Range as Range
import Domain.RingList (RingList)
import Domain.RingList as RingList

newtype Id = MkId Int

newtype MealSchedule = MkMealSchedule
  { id :: Id, startDate :: Date, schedule :: RingList PlannedMeal }

toList :: Range Date -> MealSchedule -> List PlannedMeal
toList dateRange (MkMealSchedule { startDate, schedule })
  | Range.start dateRange < startDate = Nil
  -- TODO: This needs to account for an offset
  | otherwise = RingList.toList range schedule
      where
      rangeStart = Range.start dateRange
      rangeEnd = Range.end dateRange
      Days rangeDiff = Date.diff rangeEnd rangeStart
      Days indexStart = Date.diff rangeStart startDate
      range = Range.create (Int.round indexStart) $
        (Int.round $ indexStart + rangeDiff)

-- where
-- Days days = Date.diff date startDate
-- wholeDays = (Int.ceil days) + 1

asList :: MealSchedule -> List PlannedMeal
asList (MkMealSchedule { schedule }) = RingList.asList schedule
