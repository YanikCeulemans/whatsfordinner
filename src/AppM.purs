module AppM where

import Prelude

import Capabilities.Resource.Grocery (class ManageGrocery)
import Data.Argonaut as A
import Domain.Grocery (Grocery)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype AppM a = AppM (Aff a)

runAppM :: AppM ~> Aff
runAppM (AppM aff) = aff

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM

instance ManageGrocery AppM where
  upsertGrocery :: Grocery -> AppM Unit
  upsertGrocery grocery = AppM do
    -- let
    --   serializedG = A.stringify $ A.encodeJson grocery
    pure unit
