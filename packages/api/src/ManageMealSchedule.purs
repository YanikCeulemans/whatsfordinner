module Api.ManageMealSchedule where

import Prelude

import Common.MealSchedule (MealSchedule)
import Common.MealScheduleId (MealScheduleId)
import Data.Maybe (Maybe)

class Monad m <= ManageMealSchedule m where
  loadMealSchedule :: MealScheduleId -> m (Maybe MealSchedule)

