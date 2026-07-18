module Common.Types
  ( DayPlan(..)
  , Schedule
  ) where

import Common.RingList (RingList)

data DayPlan a
  = NothingPlanned
  | Planned a

newtype Schedule a = MkSchedule (RingList a)
