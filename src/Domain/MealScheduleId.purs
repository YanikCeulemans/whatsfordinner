module Domain.MealScheduleId (MealScheduleId) where

import Domain.Id (Id)

data MealSchedule

newtype MealScheduleId = MealScheduleId (Id MealSchedule)
