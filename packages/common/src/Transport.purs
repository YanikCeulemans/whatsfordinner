module Common.Transport
  ( class Transport
  , send
  , receive
  , registeredSend
  ) where

import Effect.Class.Console
import Prelude

import Common.DevEx as DevEx
import Common.Id (Id)
import Common.Id as Id
import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either as Either
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Aff (Aff, ParAff)
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

race :: forall a f. Foldable f => f (Aff a) -> Maybe (Aff a)
race =
  map Aff.parallel
    >>> foldl help Nothing
    >>> map Aff.sequential
  where
  help :: Maybe (ParAff a) -> ParAff a -> Maybe (ParAff a)
  help acc curr =
    case acc of
      Nothing -> Just curr
      Just x -> Just $ x <|> curr

registeredSend
  :: forall a
   . Transport a
  => Id.GenEnv
  -> Json
  -> a
  -> Aff Unit
registeredSend genEnv payload transport =
  Aff.sequential $
    Aff.parallel (registeredSendHelp genEnv payload transport)
      <|> Aff.parallel (Aff.delay $ Aff.Milliseconds 500.0)

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
