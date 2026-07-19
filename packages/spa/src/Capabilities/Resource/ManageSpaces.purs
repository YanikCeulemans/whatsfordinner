module Spa.Capabilities.Resource.ManageSpaces where

import Prelude

import Common.Space (Space)
import Common.SpaceId (SpaceId)
import Data.Maybe (Maybe)
import Halogen as H

class Monad m <= ManageSpaces m where
  loadSpaces :: m (Array Space)
  loadSpace :: SpaceId -> m (Maybe Space)

instance ManageSpaces m => ManageSpaces (H.HalogenM st act slots msg m) where
  loadSpaces = H.lift loadSpaces
  loadSpace = H.lift <<< loadSpace
