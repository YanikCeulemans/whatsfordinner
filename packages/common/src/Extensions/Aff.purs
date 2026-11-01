module Common.Extensions.Aff (race) where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, ParAff)
import Effect.Aff as Aff

race :: forall a f. Foldable f => f (Aff a) -> Maybe (Aff a)
race affs =
  affs
    # foldl help Nothing
    # map Aff.sequential
  where
  help :: Maybe (ParAff a) -> Aff a -> Maybe (ParAff a)
  help acc curr =
    case acc of
      Nothing -> Just $ Aff.parallel curr
      Just x -> Just (x <|> Aff.parallel curr)

