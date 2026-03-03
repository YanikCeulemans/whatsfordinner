module Domain.Types
  ( DayPlan(..)
  , Schedule
  ) where

import Domain.RingList (RingList)

data DayPlan a
  = NothingPlanned
  | Planned a

newtype Schedule a = MkSchedule (RingList a)

