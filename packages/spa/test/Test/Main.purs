module Spa.Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

spec :: Spec Unit
spec = do
  describe "Spa tests" do
    it "works" do
      1 `shouldEqual` 1

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
