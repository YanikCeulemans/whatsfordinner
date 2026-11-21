module Common.Test.Transport where

import Prelude

import Common.Transport (class Transport, receive)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as Avar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype TestTransport = MkTestTransport (AVar (NonEmptyArray Json))

type Plucked a = { plucked :: a, rest :: Array a }

pluck
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArray a
  -> Maybe (Plucked a)
pluck predicate nonEmpty =
  help [] nonEmpty
  where
  help nonMatching xs =
    case NonEmptyArray.uncons xs of
      { head, tail } | predicate head ->
        Just { plucked: head, rest: nonMatching <> tail }
      { head, tail } -> do
        nonEmptyTail <- NonEmptyArray.fromArray tail
        help (Array.snoc nonMatching head) nonEmptyTail

instance Transport TestTransport where
  send payload (MkTestTransport var) = do
    queue <- Avar.take var
    let
      updatedQueue = NonEmptyArray.snoc queue payload
    Avar.put updatedQueue var

  receive transport@(MkTestTransport var) predicate = do
    queue <- Avar.take var

    case pluck predicate queue of
      Just { plucked, rest } -> do
        case NonEmptyArray.fromArray rest of
          Just updatedQueue -> Avar.put updatedQueue var
          Nothing -> pure unit
        pure plucked
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
