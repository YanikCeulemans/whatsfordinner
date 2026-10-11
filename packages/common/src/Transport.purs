module Common.Transport where

import Prelude

import Common.DevEx as DevEx
import Common.Id (Id)
import Common.Id as Id
import Data.Argonaut (Json)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
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

wrapInEnvelope :: forall a. Id.GenEnv -> a -> Aff (Envelope a)
wrapInEnvelope genEnv payload = do
  id <- genId genEnv
  pure { id, payload }

registeredSend
  :: forall a
   . Transport a
  => Id.GenEnv
  -> Json
  -> a
  -> Aff Unit
registeredSend genEnv payload transport = do
  id <- genId genEnv
  let
    envelope = { id, payload }
    envelopePayload = CA.encode codec envelope
  Aff.makeAff \cb -> do
    Aff.runAff_ (handleReceive cb) $ receive transport resolve
    mempty
  send envelopePayload transport
  where
  resolve _ = DevEx.todo ""
  codec = envelopeCodec CA.json
  handleReceive cb _ = do
    let
      _ = DevEx.todo ""
    liftEffect $ cb $ pure unit
    pure unit
