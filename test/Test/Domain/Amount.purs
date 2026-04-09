module Test.Domain.Amount (spec) where

import Prelude

import Data.Argonaut as J
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

encode :: Amount -> J.Json
encode = CA.encode Amount.codec

decode :: J.Json -> Either CA.JsonDecodeError Amount
decode = CA.decode Amount.codec

parseJson :: String -> J.Json
parseJson = J.parseJson >>> Either.fromRight' crash
  where
  crash _ = unsafeCrashWith "invalid hardcoded json"

codecSpec :: Spec Unit
codecSpec =
  describe "codec" do
    it "serialisation works for unitless amount" do
      let
        actual = J.stringify $ encode $ Amount.unitless 1.0

      actual `shouldEqual` """{"value":1}"""

    it "serialisation works for amount with unit" do
      let
        actual = J.stringify $ encode $ Amount.create 1.0 "kg"

      actual `shouldEqual` """{"unit":"kg","value":1}"""

    it "deserialization works for unitless amount" do
      let
        actual = decode $ parseJson """{ "value": 1 }"""
      actual `shouldContain` (Amount.unitless 1.0)

    it "deserialization works for unitless negative amount" do
      let
        actual = decode $ parseJson """{ "value": -1 }"""
      actual `shouldContain` (Amount.unitless 1.0)

    it "deserialization works for amount with unit" do
      let
        actual = decode $ parseJson """{ "value": 2, "unit": "kg" }"""
      actual `shouldContain` (Amount.create 2.0 "kg")

    deserializationOfTypeMismatchFails """[]"""
    deserializationOfTypeMismatchFails """{}"""
    deserializationOfTypeMismatchFails """{ "value" : 2, "unit": 1 }"""

deserializationOfTypeMismatchFails :: String -> Spec Unit
deserializationOfTypeMismatchFails json =
  it ("deserialization fails for type mismatching json: " <> json) do
    let
      actual = decode $ parseJson json
    actual `shouldSatisfy` Either.isLeft

spec :: Spec Unit
spec =
  describe "Amount" do
    codecSpec

