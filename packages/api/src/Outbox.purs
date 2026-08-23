module Api.Outbox where

import Prelude

import Common.DevEx (todo)
import Common.Message (Message)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class
  ( class MonadBracket
  , BracketCondition(..)
  , bracket
  )
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Aff, Fiber)
import Effect.Exception (Error, try)

class Monad m <= ManageMessages m where
  readMessage :: m Message
  ackMessage :: Message -> m Unit
  nackMessage :: Message -> m Unit

newtype OutboxApp a = OutboxApp (Aff a)

runOutboxApp :: OutboxApp ~> Aff
runOutboxApp (OutboxApp x) = x

derive newtype instance Functor OutboxApp
derive newtype instance Apply OutboxApp
derive newtype instance Bind OutboxApp
derive newtype instance Applicative OutboxApp
derive newtype instance Monad OutboxApp
derive newtype instance MonadThrow Error OutboxApp
derive newtype instance MonadError Error OutboxApp
derive newtype instance MonadBracket Error Fiber OutboxApp

instance ManageMessages OutboxApp where
  readMessage =
    todo "read from db"
  ackMessage =
    todo "ack message"
  nackMessage =
    todo "nack message"

handleMessage :: Message -> OutboxApp Unit
handleMessage _ = todo "handleMessage"

cleanupMessage :: BracketCondition Error Unit -> Message -> OutboxApp Unit
cleanupMessage condition message =
  case condition of
    Completed _ -> ackMessage message
    Failed _e -> nackMessage message
    Killed _e -> nackMessage message

run :: Aff Unit
run =
  forever $ runOutboxApp do
    bracket readMessage cleanupMessage handleMessage
