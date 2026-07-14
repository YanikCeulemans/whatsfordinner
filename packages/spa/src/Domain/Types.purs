module Spa.Domain.Types
  ( DayPlan(..)
  , Schedule
  ) where

import Spa.Domain.RingList (RingList)

data DayPlan a
  = NothingPlanned
  | Planned a

newtype Schedule a = MkSchedule (RingList a)
