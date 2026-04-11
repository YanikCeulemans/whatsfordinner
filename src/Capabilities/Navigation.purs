module Capabilities.Navigation where

import Prelude

import Data.Route (Route)
import Halogen as H

class Monad m <= Navigation m where
  navigate :: Route -> m Unit

instance
  Navigation m =>
  Navigation (H.HalogenM st act slots msg m) where
  navigate = H.lift <<< navigate
