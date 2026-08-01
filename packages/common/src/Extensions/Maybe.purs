module Common.Extensions.Maybe where

import Prelude

import Data.Maybe (Maybe(..))

orElse :: forall a. (Unit -> Maybe a) -> Maybe a -> Maybe a
orElse whenNothing = case _ of
  Nothing -> whenNothing unit
  Just x -> Just x
