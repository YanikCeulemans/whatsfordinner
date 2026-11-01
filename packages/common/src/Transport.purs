module Common.Transport
  ( class Transport
  , send
  , receive
  , registeredSend
  ) where

import Prelude

import Common.Extensions.Aff as AffE
import Common.Id (Id)
import Common.Id as Id
import Data.Argonaut (Json)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either as Either
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)

class Transport a where
  send :: Json -> a -> Aff Unit
  receive :: a -> (Json -> Boolean) -> Aff Json

data Envelope_
type EnvelopeId = Id Envelope_

genId :: Id.GenEnv -> Aff EnvelopeId
genId = Id.gen >>> liftEffect

-- | A wrapper type for payloads with an id attached
type Envelope a =
  { id :: EnvelopeId
  , payload :: a
  }

envelopeCodec
  :: forall a. JsonCodec a -> JsonCodec (Envelope a)
envelopeCodec payloadCodec =
  CA.object "envelope"
    $ CAR.record
        { id: Id.codec
        , payload: payloadCodec
        }

registeredSend
  :: forall a
   . Transport a
  => Id.GenEnv
  -> Json
  -> a
  -> Aff Unit
registeredSend genEnv payload transport =
  NonEmptyArray
    [ registeredSendHelp genEnv payload transport
    , Aff.delay $ Aff.Milliseconds 500.0
    ]
    # AffE.race

registeredSendHelp
  :: forall a
   . Transport a
  => Id.GenEnv
  -> Json
  -> a
  -> Aff Unit
registeredSendHelp genEnv payload transport = do
  id <- genId genEnv
  let
    envelope = { id, payload }
    envelopePayload = CA.encode codec envelope
  receiveFiber <- Aff.forkAff $ receive transport $ matchesEnvelopeId id
  send envelopePayload transport
  void $ Aff.joinFiber receiveFiber
  where
  codec = envelopeCodec CA.json

  matchesEnvelopeId id received =
    CA.decode codec received
      <#> _.id >>> eq id
      # Either.fromRight false
