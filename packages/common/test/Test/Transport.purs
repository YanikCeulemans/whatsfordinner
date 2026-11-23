module Common.Test.Transport where

import Prelude

import Common.Id as Id
import Common.Transport (class Transport, receive, registeredSend, send)
import Data.Argonaut (Json, jsonNull)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype TestTransport = MkTestTransport (AVar (NonEmptyArray Json))

createEmptyTransport :: Aff TestTransport
createEmptyTransport = MkTestTransport <$> AVar.empty

type Plucked a = { plucked :: a, rest :: Array a }

pluck
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArray a
  -> Maybe (Plucked a)
pluck predicate nonEmpty = do
  let
    { init, rest } = NonEmptyArray.span (not <<< predicate) nonEmpty
  { head, tail } <- Array.uncons rest
  pure { plucked: head, rest: init <> tail }

instance Transport TestTransport where
  send payload (MkTestTransport var) = do
    queue <- AVar.tryTake var
    let
      updatedQueue =
        queue
          # map (\q -> NonEmptyArray.snoc q payload)
          # Maybe.fromMaybe' (\_ -> NonEmptyArray.cons' payload [])

    AVar.put updatedQueue var

  receive transport@(MkTestTransport var) predicate = do
    queue <- AVar.take var

    case pluck predicate queue of
      Just { plucked, rest } -> do
        case NonEmptyArray.fromArray rest of
          Just updatedQueue -> AVar.put updatedQueue var
          Nothing -> pure unit
        pure plucked
      Nothing -> tryAgain queue

    where
    tryAgain queue = do
      AVar.put queue var
      Aff.delay $ Aff.Milliseconds 50.0
      receive transport predicate

registeredSendSpec :: Spec Unit
registeredSendSpec = do
  describe "registeredSend" do
    it "should work" do
      transport <- createEmptyTransport
      forked <- Aff.forkAff do
        payload <- receive transport (const true)
        send payload transport
      actual <- registeredSend Id.Node jsonNull transport
      Aff.joinFiber forked
      actual `shouldEqual` unit

spec :: Spec Unit
spec = do
  registeredSendSpec
