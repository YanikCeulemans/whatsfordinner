module Common.Extensions.Aff (race, timeout) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foldable (oneOf)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Time.Duration (class Duration)
import Data.Time.Duration as Duration
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)

race :: forall a. NonEmpty Array (Aff a) -> Aff a
race affs =
  affs
    # map Aff.parallel
    # oneOf
    # Aff.sequential

data TimeoutResult a
  = CompletedInTime a
  | HitDeadline

timeout
  :: forall a d
   . Duration d
  => d
  -> Aff a
  -> Aff a
timeout duration action = do
  result <- race $ deadline :| [ CompletedInTime <$> action ]
  case result of
    CompletedInTime x -> pure x
    HitDeadline -> throwError $ Aff.error $ "timeout hit the deadline: " <>
      show milliseconds
  where
  milliseconds = Duration.convertDuration duration
  deadline = do
    liftAff $ Aff.delay $ milliseconds
    pure HitDeadline
