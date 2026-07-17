module Spa.Capabilities.Navigation where

import Prelude

import Halogen as H
import Spa.Data.Route (Route)

class Monad m <= Navigation m where
  navigate :: Route -> m Unit

instance
  Navigation m =>
  Navigation (H.HalogenM st act slots msg m) where
  navigate = H.lift <<< navigate
