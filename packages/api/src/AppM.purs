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

type Env = ({ spaces :: AVar (Map SpaceId Space) })

newtype AppM a = MkAppM (ReaderT Env Aff a)

runAppM :: forall a. Env -> AppM a -> Aff a
runAppM state (MkAppM app) = runReaderT app state

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Applicative AppM
derive newtype instance Monad AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAsk Env AppM

loadSpaceFromMemory :: SpaceId -> AppM (Maybe Space)
loadSpaceFromMemory spaceId = do
  env <- ask
  spaces <- liftAff $ AVar.read env.spaces
  pure $ Map.lookup spaceId spaces

upsertSpaceFromMemory :: SpaceId -> Space -> AppM Unit
upsertSpaceFromMemory spaceId space = do
  env <- ask
  spaces <- liftAff $ AVar.take env.spaces
  let
    updatedSpaces = Map.insert spaceId space spaces
  liftAff $ AVar.put updatedSpaces env.spaces

instance ManageSpaces AppM where
  loadSpace = loadSpaceFromMemory
  upsertSpace = upsertSpaceFromMemory
