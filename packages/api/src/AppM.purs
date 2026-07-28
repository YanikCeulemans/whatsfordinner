module Api.AppM where

import Prelude

import Api.ManageSpaces (class ManageSpaces)
import Common.Space (Space)
import Common.SpaceId (SpaceId)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception.Unsafe (unsafeThrow)

type State = ({ spaces :: AVar (Map SpaceId Space) })

newtype AppM a = MkAppM (ReaderT State Aff a)

runAppM :: forall a. State -> AppM a -> Aff a
runAppM state (MkAppM app) = runReaderT app state

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Applicative AppM
derive newtype instance Monad AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAsk State AppM

loadSpaceFromMemory :: SpaceId -> AppM (Maybe Space)
loadSpaceFromMemory spaceId = do
  state <- ask
  spaces <- liftAff $ AVar.read state.spaces
  pure $ Map.lookup spaceId spaces

instance ManageSpaces AppM where
  loadSpace = loadSpaceFromMemory
