module Capabilities.Resource.ManageSpaces where

import Prelude

import Domain.Space (Space)
import Halogen as H

class Monad m <= ManageSpaces m where
  loadSpaces :: m (Array Space)

instance ManageSpaces m => ManageSpaces (H.HalogenM st act slots msg m) where
  loadSpaces = H.lift loadSpaces
