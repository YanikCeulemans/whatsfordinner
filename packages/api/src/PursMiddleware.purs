module Api.PursMiddleware where

import Prelude

import Effect (Effect)
import Effect.Aff.Compat (mkEffectFn3, runEffectFn1)
import Effect.Exception (Error)
import HTTPurple (NodeMiddleware(..))
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Untagged.Union (UndefinedOr)

type PursMiddleware =
  (IncomingMessage IMServer)
  -> ServerResponse
  -> (UndefinedOr Error -> Effect Unit)
  -> Effect Unit

pursMiddleware
  :: PursMiddleware
  -> NodeMiddleware ()
pursMiddleware middleware = NodeMiddleware $ mkEffectFn3 help
  where
  next' n = runEffectFn1 n
  help request response next = do
    middleware request response $ next' next
    pure $ pure unit

