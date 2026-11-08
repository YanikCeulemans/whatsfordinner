module Common.Test.Extensions.Aff where

import Prelude

import Common.Extensions.Aff as AffE
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Error)
import Effect.Aff as Aff
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, fail, shouldEqual, shouldSatisfy)

-- | Asserts that the action throws and that its error message contains the
-- | given pattern
shouldThrowContaining
  :: forall m a
   . Show a
  => MonadError Error m
  => m a
  -> String
  -> m Unit
shouldThrowContaining actual pattern = do
  e <- Aff.try actual
  case e of
    Left err ->
      unless (String.contains (String.Pattern pattern) $ Aff.message err)
        $ fail
        $ "Error message ∌ " <> pattern
    Right x -> fail $ "expected action to throw, instead got: " <> show x

shouldThrowSatisfying
  :: forall m a
   . Show a
  => MonadError Error m
  => m a
  -> (Error -> Boolean)
  -> m Unit
shouldThrowSatisfying actual predicate = do
  e <- Aff.try actual
  case e of
    Left err -> err `shouldSatisfy` predicate
    Right x -> fail $ "expected action to throw, instead got: " <> show x

timeoutSpec :: Spec Unit
timeoutSpec =
  describe "timeout" do
    it "instant action wins deadline" do
      actual <- AffE.timeout (Milliseconds 10.0) (pure true)

      actual `shouldEqual` true

    it "fast enough action wins deadline" do
      actual <- AffE.timeout (Milliseconds 10.0)
        (Aff.delay (Milliseconds 5.0) $> true)

      actual `shouldEqual` true

    it "slow action misses deadline" do
      let
        actual = AffE.timeout (Milliseconds 10.0)
          (Aff.delay (Milliseconds 200.0) $> true)

      expectError actual

    it "missing a deadline has the deadline duration in its error message" do
      let
        deadlineDuration = (Milliseconds 10.0)
        actual = AffE.timeout deadlineDuration
          (Aff.delay (Milliseconds 200.0) $> true)

      actual `shouldThrowContaining` show deadlineDuration

spec :: Spec Unit
spec =
  describe "Aff extensions"
    timeoutSpec
