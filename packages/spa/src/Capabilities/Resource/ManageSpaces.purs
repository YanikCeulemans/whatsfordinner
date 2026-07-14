module Spa.Capabilities.Resource.ManageSpaces where

import Prelude

import Data.Maybe (Maybe)
import Spa.Domain.Space (Space)
import Spa.Domain.SpaceId (SpaceId)
import Halogen as H

class Monad m <= ManageSpaces m where
  loadSpaces :: m (Array Space)
  loadSpace :: SpaceId -> m (Maybe Space)

instance ManageSpaces m => ManageSpaces (H.HalogenM st act slots msg m) where
  loadSpaces = H.lift loadSpaces
  loadSpace = H.lift <<< loadSpace
