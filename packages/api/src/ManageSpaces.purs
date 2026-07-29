module Api.ManageSpaces where

import Prelude

import Common.Space (Space)
import Common.SpaceId (SpaceId)
import Data.Maybe (Maybe)

class Monad m <= ManageSpaces m where
  loadSpace :: SpaceId -> m (Maybe Space)
  upsertSpace :: SpaceId -> Space -> m Unit
