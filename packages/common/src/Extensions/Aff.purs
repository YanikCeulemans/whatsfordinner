module Common.Extensions.Aff (race) where

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Effect.Aff (Aff, ParAff)
import Effect.Aff as Aff

race :: forall a. NonEmptyArray (Aff a) -> Aff a
race affs =
  affs
    # map Aff.parallel
    # NonEmptyArray.foldl1 help
    # Aff.sequential
  where
  help :: ParAff a -> ParAff a -> ParAff a
  help acc curr =
    acc <|> curr

