module Api.Outbox where

import Prelude

import Api.Publisher (class Publisher, publishCodec, publishEncode)
import Api.WS (WebSocket)
import Api.WS as WS
import Common.DevEx (todo)
import Common.GroceryListId (GroceryListId)
import Common.Id (Id)
import Common.Message (Message)
import Common.Message as Message
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class
  ( class MonadBracket
  , class MonadFork
  , class MonadKill
  , BracketCondition(..)
  , bracket
  , kill
  , never
  , uninterruptible
  )
import Control.Monad.Reader
  ( class MonadAsk
  , ReaderT(..)
  , asks
  , lift
  , runReaderT
  )
import Control.Monad.Reader.Class (ask)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut (stringify)
import Data.Lens as Lens
import Data.Lens.Record as LensRecord
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Traversable (for_)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Prelude (Proxy(..))

class Monad m <= ManageMessages m where
  readMessage :: m Message
  ackMessage :: Message -> m Unit
  nackMessage :: Message -> m Unit

type OutboxEnvInner =
  { sockets :: Ref (Map GroceryListId (Map (Id WebSocket) WebSocket))
  , outbox :: AVar (List Message)
  }

newtype OutboxEnv = OutboxEnv OutboxEnvInner

_outboxEnv :: Lens.Lens' OutboxEnv OutboxEnvInner
_outboxEnv = Lens.iso unwrap wrap
  where
  wrap = OutboxEnv
  unwrap (OutboxEnv x) = x

_sockets
  :: Lens.Lens' OutboxEnv
       (Ref (Map GroceryListId (Map (Id WebSocket) WebSocket)))
_sockets = _outboxEnv <<< LensRecord.prop (Proxy @"sockets")

_outbox :: Lens.Lens' OutboxEnv (AVar (List Message))
_outbox = _outboxEnv <<< LensRecord.prop (Proxy @"outbox")

lookupSockets :: OutboxEnv -> GroceryListId -> Aff (List WebSocket)
lookupSockets outboxEnv groceryListId =
  Lens.view _sockets outboxEnv
    # Ref.read
    # liftEffect
    <#> Map.lookup groceryListId
      >>> Maybe.fromMaybe Map.empty
      >>> Map.values

newtype OutboxApp a = OutboxApp (ReaderT OutboxEnv Aff a)

runOutboxApp :: forall a. OutboxEnv -> OutboxApp a -> Aff a
runOutboxApp env (OutboxApp x) = runReaderT x env

derive newtype instance Functor OutboxApp
derive newtype instance Apply OutboxApp
derive newtype instance Bind OutboxApp
derive newtype instance Applicative OutboxApp
derive newtype instance Monad OutboxApp
derive newtype instance MonadThrow Error OutboxApp
derive newtype instance MonadError Error OutboxApp
derive newtype instance MonadAsk OutboxEnv OutboxApp

derive newtype instance MonadFork Fiber OutboxApp
instance MonadKill Error Fiber OutboxApp where
  kill err fiber = OutboxApp $ lift $ kill err fiber

instance MonadBracket Error Fiber OutboxApp where
  bracket acquire release use = OutboxApp $ bracket acquire' release' use'
    where
    OutboxApp acquire' = acquire
    release' condition resource = case release condition resource of
      OutboxApp r -> r
    use' resource = case use resource of OutboxApp r -> r
  never = OutboxApp $ lift $ never
  uninterruptible (OutboxApp x) = OutboxApp $ uninterruptible x

instance ManageMessages OutboxApp where
  readMessage = OutboxApp do
    env <- ask
    Lens.view _outbox env
      # AVar.take
    todo "read from db"
  ackMessage =
    todo "ack message"
  nackMessage =
    todo "nack message"

instance Publisher GroceryListId OutboxApp where
  publishJson id json = OutboxApp do
    env <- ask
    sockets <- liftAff $ lookupSockets env id
    for_ sockets \socket -> do
      -- TODO: we probably want at least once delivery here
      liftEffect $ WS.send payload socket
    where
    payload = stringify json

handleMessage :: Message -> OutboxApp Unit
handleMessage message =
  publishEncode groceryListId message
  where
  groceryListId = Message.groceryListId message

cleanupMessage :: BracketCondition Error Unit -> Message -> OutboxApp Unit
cleanupMessage condition message =
  case condition of
    Completed _ -> ackMessage message
    Failed _e -> nackMessage message
    Killed _e -> nackMessage message

run :: OutboxEnv -> Aff Unit
run env =
  forever $ runOutboxApp env do
    bracket readMessage cleanupMessage handleMessage
