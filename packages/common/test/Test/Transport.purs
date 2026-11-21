module Common.Test.Transport where

import Prelude

import Common.Transport (class Transport, receive)
import Data.Argonaut (Json)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as Avar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype TestTransport = MkTestTransport (AVar (NonEmptyArray Json))

pluck predicate nonEmpty =
  case NonEmptyArray.uncons nonEmpty of
    { head, tail } | predicate head ->
      Just { plucked: head, rest: NonEmptyArray.fromArray tail }

instance Transport TestTransport where
  send payload (MkTestTransport var) = do
    queue <- Avar.take var
    let
      updatedQueue = NonEmptyArray.snoc queue payload
    Avar.put updatedQueue var

  receive transport@(MkTestTransport var) predicate = do
    queue <- Avar.take var

    case findPayload queue of
      NonEmptyArray head tail | predicate head -> do
        case Array.uncons tail of
          Nothing -> pure unit
          Just uncons -> Avar.put (NonEmptyArray uncons.head uncons.tail) var
        pure head

      NonEmptyArray _ _ -> tryAgain queue

    where
    -- findPayload (NonEmptyArray head tail)
    --   | predicate head = Just head
    --   | otherwise = NonEmptyArray.fromArray tail >>= findPayload

    pluckPayload nonMatching nonEmpty =
      case NonEmptyArray.uncons nonEmpty of
        { head, tail } | predicate head ->
          Just
            { payload: head
            , rest: NonEmptyArray.fromArray $ nonMatching <> tail
            }
        { head, tail } -> do
          nonEmptyTail <- NonEmptyArray.fromArray tail
          pluckPayload (Array.snoc nonMatching head) nonEmptyTail

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
