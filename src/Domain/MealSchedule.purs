module Domain.MealSchedule where

import Prelude

import Data.Date (Date)
import Data.Date as Date
import Data.Int as Int
import Data.List (List(..))
import Data.Time.Duration (Days(..))
import Domain.PlannedMeal (PlannedMeal)
import Domain.RingList (RingList)
import Domain.RingList as RingList

newtype Id = MkId Int

newtype MealSchedule = MkMealSchedule
  { id :: Id, startDate :: Date, schedule :: RingList PlannedMeal }

toList :: Date -> MealSchedule -> List PlannedMeal
toList date (MkMealSchedule { startDate, schedule })
  | date < startDate = Nil
  | otherwise = RingList.toListWithRange wholeDays schedule
      where
      Days days = Date.diff date startDate
      wholeDays = (Int.ceil days) + 1

asList :: MealSchedule -> List PlannedMeal
asList (MkMealSchedule { schedule }) = RingList.asList schedule
