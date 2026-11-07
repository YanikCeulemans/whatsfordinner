module Common.Extensions.Aff (race, timeout) where

import Prelude

import Control.Alt (alt)
import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Time.Duration (class Duration)
import Data.Time.Duration as Duration
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Exception as Error

race :: forall a. NonEmptyArray (Aff a) -> Aff a
race affs =
  affs
    # map Aff.parallel
    # NonEmptyArray.foldl1 alt
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
  result <- race $ NonEmptyArray.cons' deadline [ CompletedInTime <$> action ]
  case result of
    CompletedInTime x -> pure x
    HitDeadline -> throwError $ Error.error $ "timeout hit the deadline: " <>
      show milliseconds
  where
  milliseconds = Duration.convertDuration duration
  deadline = do
    liftAff $ Aff.delay $ milliseconds
    pure HitDeadline
