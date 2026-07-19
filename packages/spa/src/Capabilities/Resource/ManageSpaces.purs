module Spa.Capabilities.Resource.ManageSpaces where

import Prelude

import Data.Maybe (Maybe)
import Halogen as H
import Common.Space (Space)
import Common.SpaceId (SpaceId)

class Monad m <= ManageSpaces m where
  loadSpaces :: m (Array Space)
  loadSpace :: SpaceId -> m (Maybe Space)

instance ManageSpaces m => ManageSpaces (H.HalogenM st act slots msg m) where
  loadSpaces = H.lift loadSpaces
  loadSpace = H.lift <<< loadSpace
