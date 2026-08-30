module Api.Outbox where

import Prelude

import Api.Publisher (class Publisher, defaultPublish)
import Api.WS (WebSocket)
import Common.DevEx (todo)
import Common.GroceryListId (GroceryListId)
import Common.Id (Id)
import Common.Message (Message)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class
  ( class MonadBracket
  , BracketCondition(..)
  , bracket
  )
import Control.Monad.Reader (class MonadAsk, ReaderT(..), asks, runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Lens as Lens
import Data.Lens.Record as LensRecord
import Data.Map (Map)
import Effect.Aff (Aff, Fiber)
import Effect.Exception (Error, try)
import Effect.Ref (Ref)
import Type.Prelude (Proxy(..))

class Monad m <= ManageMessages m where
  readMessage :: m Message
  ackMessage :: Message -> m Unit
  nackMessage :: Message -> m Unit

type OutboxEnvInner =
  { sockets :: Ref (Map GroceryListId (Map (Id WebSocket) WebSocket)) }

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
derive newtype instance MonadBracket Error Fiber OutboxApp
derive newtype instance MonadAsk OutboxEnv OutboxApp

instance ManageMessages OutboxApp where
  readMessage =
    todo "read from db"
  ackMessage =
    todo "ack message"
  nackMessage =
    todo "nack message"

instance Publisher OutboxApp where
  publish = defaultPublish
  publishJson json = OutboxApp do
    todo "publish"

handleMessage :: Message -> OutboxApp Unit
handleMessage _ = todo "handleMessage"

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
