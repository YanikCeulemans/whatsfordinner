module Common.Test.Grocery (spec) where

import Prelude

import Common.Amount as Amount
import Common.Extensions.ULID as ULIDExt
import Common.GroceryEntryId (GroceryEntryId)
import Common.GroceryList (GroceryEntry)
import Common.GroceryList as GroceryList
import Common.Id as Id
import Data.Argonaut as J
import Data.Array (fold)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Data.Tuple (fst)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain)

rawGroceryId :: String
rawGroceryId = "01KNEQ7KMSBM0Q4XP56C6YP3NG"

groceryId :: GroceryEntryId
groceryId = Id.MkId
  $ Either.fromRight' crash
  $ ULIDExt.parse rawGroceryId
  where
  crash _ = unsafeCrashWith "invalid hardcoded ulid"

quoted :: String -> String
quoted x = fold [ "\"", x, "\"" ]

decode :: J.Json -> Either CA.JsonDecodeError GroceryEntry
decode = CA.decode GroceryList.entryCodec

parseJson :: String -> J.Json
parseJson = J.parseJson >>> Either.fromRight' crash
  where
  crash _ = unsafeCrashWith "invalid hardcoded json"

grocery :: GroceryEntry
grocery =
  GroceryList.upsertGrocery groceryId "Tomatoes" (Amount.unitless 1.0) mempty
    # fst

codecSpec :: Spec Unit
codecSpec =
  describe "codec" do
    it "deserialization works for grocery" do
      let
        actual = decode $ parseJson
          ( fold
              [ """{"amount":{"tag": "unitless", "value":1},"checked":false,"description":"Tomatoes","id":"""
              , quoted rawGroceryId
              , ""","sortIndex": 0}"""
              ]
          )
      actual `shouldContain` grocery

spec :: Spec Unit
spec =
  describe "Grocery" do
    codecSpec
