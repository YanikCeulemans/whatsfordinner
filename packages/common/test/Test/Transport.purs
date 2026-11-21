module Common.Test.Transport where

import Prelude

import Common.Transport (class Transport, receive)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as Avar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype TestTransport = MkTestTransport (AVar (Array Json))

instance Transport TestTransport where
  send payload (MkTestTransport var) = do
    queue <- Avar.take var
    let
      updatedQueue = Array.snoc queue payload
    Avar.put updatedQueue var
  receive transport@(MkTestTransport var) predicate = do
    queue <- Avar.take var
    case Array.uncons queue of
      Just { head, tail } | predicate head -> do
        Avar.put tail var
        pure head
      Just _ -> tryAgain queue
      Nothing -> tryAgain queue
    where
    tryAgain queue = do
      Avar.put queue var
      Aff.delay $ Aff.Milliseconds 50.0
      receive transport predicate

registeredSendSpec :: Spec Unit
registeredSendSpec = do
  describe "registeredSend" do
    it "should work" do
      true `shouldEqual` false

spec :: Spec Unit
spec = do
  registeredSendSpec
