module Capabilities.Resource.Grocery where

import Prelude

import Domain.Grocery (Grocery)
import Halogen as H

class Monad m <= ManageGrocery m where
  upsertGrocery :: Grocery -> m Unit

instance ManageGrocery m => ManageGrocery (H.HalogenM st act slots msg m) where
  upsertGrocery = H.lift <<< upsertGrocery
