module Test.Domain.GroceryId (spec) where

import Prelude

import Data.Argonaut as J
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either as Either
import Data.ULID as DULID
import Domain.GroceryId (GroceryId)
import Domain.GroceryId as GroceryId
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

rawGroceryId :: String
rawGroceryId = "01KNEQ7KMSBM0Q4XP56C6YP3NG"

groceryId :: GroceryId
groceryId = GroceryId.MkGroceryId
  $ Either.fromRight' crash
  $ DULID.parse rawGroceryId
  where
  crash _ = unsafeCrashWith "invalid hardcoded ulid"

encode :: GroceryId -> J.Json
encode = CA.encode GroceryId.groceryIdCodec

decode :: J.Json -> Either CA.JsonDecodeError GroceryId
decode = CA.decode GroceryId.groceryIdCodec

quoted :: String -> String
quoted x = fold [ "\"", x, "\"" ]

codecSpec :: Spec Unit
codecSpec =
  describe "codec" do
    it "serialisation works" do
      let
        actual = J.stringify $ encode groceryId

      actual `shouldEqual` (quoted rawGroceryId)

    it "deserialization works" do
      let
        crash _ = unsafeCrashWith "invalid hardcoded json"
        actual = decode $ Either.fromRight' crash $ J.parseJson $ quoted
          rawGroceryId
      actual `shouldContain` groceryId

    it "deserialization fails for invalid ULID: number" do
      let
        crash _ = unsafeCrashWith "invalid hardcoded json"
        actual = decode $ Either.fromRight' crash $ J.parseJson $ "123"
      actual `shouldEqual` (Left $ CA.TypeMismatch "string")

    it "deserialization fails for invalid ULID: string" do
      let
        crash _ = unsafeCrashWith "invalid hardcoded json"
        actual = decode $ Either.fromRight' crash $ J.parseJson $ quoted "123"
      actual `shouldEqual` (Left $ CA.TypeMismatch "must be 26 chars in length")

spec :: Spec Unit
spec =
  describe "GroceryId" do
    codecSpec

