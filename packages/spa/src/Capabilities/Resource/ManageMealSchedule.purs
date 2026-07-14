module Spa.Capabilities.Resource.ManageMealSchedule where

import Prelude

import Data.Maybe (Maybe)
import Spa.Domain.MealSchedule (MealSchedule)
import Spa.Domain.MealScheduleId (MealScheduleId)
import Halogen as H

class Monad m <= ManageMealSchedule m where
  loadMealSchedule :: MealScheduleId -> m (Maybe MealSchedule)

instance
  ManageMealSchedule m =>
  ManageMealSchedule (H.HalogenM st act slots msg m) where
  loadMealSchedule = H.lift <<< loadMealSchedule
