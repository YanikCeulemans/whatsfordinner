module Capabilities.Resource.ManageMealSchedule where

import Prelude

import Data.Maybe (Maybe)
import Domain.MealSchedule (MealSchedule)
import Domain.MealScheduleId (MealScheduleId)
import Halogen as H

class Monad m <= ManageMealSchedule m where
  loadMealSchedule :: MealScheduleId -> m (Maybe MealSchedule)

instance
  ManageMealSchedule m =>
  ManageMealSchedule (H.HalogenM st act slots msg m) where
  loadMealSchedule = H.lift <<< loadMealSchedule
