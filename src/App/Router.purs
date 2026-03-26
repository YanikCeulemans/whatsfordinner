module App.Route where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Route (Route)
import Halogen as H

data Query a = Navigate Route a

component
  :: forall input output m. MonadAff m => H.Component Query input output m
component = ?h
