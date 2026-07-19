module Spa.Capabilities.Resource.ManageMealSchedule where

import Prelude

import Data.Maybe (Maybe)
import Halogen as H
import Common.MealSchedule (MealSchedule)
import Common.MealScheduleId (MealScheduleId)

class Monad m <= ManageMealSchedule m where
  loadMealSchedule :: MealScheduleId -> m (Maybe MealSchedule)

instance
  ManageMealSchedule m =>
  ManageMealSchedule (H.HalogenM st act slots msg m) where
  loadMealSchedule = H.lift <<< loadMealSchedule
