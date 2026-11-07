module Common.Test.Extensions.Aff where

import Prelude

import Common.Extensions.Aff as AffE
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

timeoutSpec :: Spec Unit
timeoutSpec =
  describe "timeout" do
    it "instant action wins deadline" do
      actual <- AffE.timeout (Milliseconds 100.0) (pure true)

      actual `shouldEqual` true

    it "slow action misses deadline" do
      actual <- AffE.timeout (Milliseconds 100.0)
        (Aff.delay (Milliseconds 200.0) $> true)

      actual `shouldEqual` true

spec :: Spec Unit
spec =
  describe "Aff extensions"
    timeoutSpec
